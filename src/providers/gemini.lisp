;;;; providers/gemini.lisp - Google Gemini provider implementation

(in-package #:clprompt)

;;; ============================================================================
;;; Gemini Provider Class
;;; ============================================================================

(defclass gemini-provider (provider api-key-mixin)
  ((model :initarg :model
          :initform "gemini-1.5-pro"
          :reader provider-model
          :documentation "Gemini model identifier.")
   (endpoint :initarg :endpoint
             :initform "https://generativelanguage.googleapis.com/v1beta"
             :reader provider-endpoint
             :documentation "Gemini API base endpoint."))
  (:documentation "Google Gemini API provider."))

(defmethod provider-name ((provider gemini-provider))
  "gemini")

(defmethod provider-env-var-name ((provider gemini-provider))
  "GOOGLE_API_KEY")

;;; ============================================================================
;;; Gemini API Implementation
;;; ============================================================================

(defmethod send-request ((provider gemini-provider) prompt &key
                         model temperature max-tokens output-schema)
  "Send a prompt to Gemini and return the response."
  (let* ((actual-model (or model (provider-model provider)))
         (api-key (provider-api-key provider))
         (url (format nil "~A/models/~A:generateContent?key=~A"
                      (provider-endpoint provider)
                      actual-model
                      api-key)))
    (unless api-key
      (error 'authentication-error
             :provider provider
             :message "No API key configured for Gemini"))
    (let ((body (build-gemini-request prompt
                                      :temperature temperature
                                      :max-tokens max-tokens
                                      :output-schema output-schema)))
      (multiple-value-bind (response status headers error-body)
          (http-post-json url body)
        (declare (ignore headers))
        (handle-gemini-response provider response status error-body)))))

(defmethod send-request-streaming ((provider gemini-provider) prompt &key
                                   on-token model temperature max-tokens output-schema
                                   &allow-other-keys)
  "Stream a prompt to Gemini, invoking ON-TOKEN for each chunk and returning the aggregated response."
  (let* ((actual-model (or model (provider-model provider)))
         (api-key (provider-api-key provider))
         (url (format nil "~A/models/~A:streamGenerateContent?key=~A"
                      (provider-endpoint provider)
                      actual-model
                      api-key)))
    (unless api-key
      (error 'authentication-error
             :provider provider
             :message "No API key configured for Gemini"))
    (let ((body (build-gemini-request prompt
                                      :temperature temperature
                                      :max-tokens max-tokens
                                      :output-schema output-schema)))
      (multiple-value-bind (stream status headers error-body)
          (http-post-json-stream url body)
        (declare (ignore headers))
        (cond
          ((and status (= status 200) stream)
           (gemini-streaming-response stream on-token))
          ((and status (= status 429))
           (error 'rate-limit-error
                  :provider provider
                  :status-code status
                  :message "Rate limit exceeded"))
          ((and status (or (= status 401) (= status 403)))
           (error 'authentication-error
                  :provider provider
                  :status-code status
                  :message "Invalid API key"))
          (t
           (error 'provider-error
                  :provider provider
                  :status-code status
                  :response-body error-body
                  :message (format nil "Gemini API error: ~A"
                                   (or (extract-error-message error-body)
                                       error-body
                                       "Unknown error")))))))))

(defun build-gemini-request (prompt &key temperature max-tokens output-schema)
  "Build the request body for Gemini API."
  (let ((request (make-hash-table :test 'equal)))
    ;; Contents
    (let ((contents (list (make-hash-table :test 'equal))))
      (let ((parts (list (make-hash-table :test 'equal))))
        (setf (gethash "text" (first parts)) prompt)
        (setf (gethash "parts" (first contents)) parts))
      (setf (gethash "role" (first contents)) "user")
      (setf (gethash "contents" request) contents))
    ;; Generation config
    (let ((gen-config (make-hash-table :test 'equal)))
      (when temperature
        (setf (gethash "temperature" gen-config) temperature))
      (when max-tokens
        (setf (gethash "maxOutputTokens" gen-config) max-tokens))
      (when output-schema
        (setf (gethash "responseMimeType" gen-config) "application/json"))
      (when (> (hash-table-count gen-config) 0)
        (setf (gethash "generationConfig" request) gen-config)))
    request))

(defun handle-gemini-response (provider response status error-body)
  "Handle the Gemini API response."
  (cond
    ;; Success
    ((and status (= status 200) response)
     (parse-gemini-response response))
    ;; Rate limit
    ((and status (= status 429))
     (error 'rate-limit-error
            :provider provider
            :status-code status
            :message "Rate limit exceeded"))
    ;; Auth error
    ((and status (or (= status 401) (= status 403)))
     (error 'authentication-error
            :provider provider
            :status-code status
            :message "Invalid API key"))
    ;; Other error
    (t
     (error 'provider-error
            :provider provider
            :status-code status
            :response-body error-body
            :message (format nil "Gemini API error: ~A"
                             (or (extract-error-message response)
                                 error-body
                                 "Unknown error"))))))

(defun parse-gemini-response (response)
  "Parse successful Gemini response into standardized format."
  (let* ((candidates (gethash "candidates" response))
         (first-candidate (and candidates (first candidates)))
         (content (and first-candidate (gethash "content" first-candidate)))
         (parts (and content (gethash "parts" content)))
         (text (and parts (gethash "text" (first parts))))
         (usage (gethash "usageMetadata" response))
         (finish-reason (and first-candidate
                             (gethash "finishReason" first-candidate))))
    (list :content text
          :raw response
          :finish-reason finish-reason
         :usage (when usage
                   (list :prompt-tokens (gethash "promptTokenCount" usage)
                         :completion-tokens (gethash "candidatesTokenCount" usage)
                         :total-tokens (gethash "totalTokenCount" usage))))))

(defun parse-gemini-stream-event (event)
  "Extract chunk text, finish reason, and usage metadata from a streaming EVENT."
  (let* ((candidates (gethash "candidates" event))
         (first-candidate (and candidates (first candidates)))
         (content (and first-candidate (gethash "content" first-candidate)))
         (parts (and content (gethash "parts" content)))
         (text (and parts (gethash "text" (first parts))))
         (finish-reason (and first-candidate
                             (gethash "finishReason" first-candidate)))
         (usage (gethash "usageMetadata" event)))
    (values text finish-reason usage)))

(defun gemini-streaming-response (stream on-token)
  "Consume Gemini SSE STREAM and build a response plist.
Calls ON-TOKEN with each text chunk when provided."
  (let ((chunks '())
        (raw-events '())
        (finish-reason nil)
        (usage nil))
    (flet ((emit (chunk)
             (when chunk
               (push chunk chunks)
               (when on-token
                 (funcall on-token chunk))))
           (build-result ()
             (list :content (apply #'concatenate 'string (nreverse chunks))
                   :raw (nreverse raw-events)
                   :finish-reason finish-reason
                   :usage (when usage
                            (list :prompt-tokens (gethash "promptTokenCount" usage)
                                  :completion-tokens (gethash "candidatesTokenCount" usage)
                                  :total-tokens (gethash "totalTokenCount" usage))))))
      (consume-sse-stream stream
        (lambda (payload)
          (cond
            ((or (null payload) (string= payload ""))
             nil)  ; Skip empty payloads
            (t
             (let ((event (yason:parse payload)))
               (push event raw-events)
               (multiple-value-bind (chunk finish usage-meta)
                   (parse-gemini-stream-event event)
                 (when chunk (emit chunk))
                 (when (and (null finish-reason) finish)
                   (setf finish-reason finish))
                 (when (and usage-meta (null usage))
                   (setf usage usage-meta))))
             nil))))
      (build-result))))

(defun extract-error-message (response)
  "Extract error message from Gemini error response."
  (when (hash-table-p response)
    (when-let (error-obj (gethash "error" response))
      (gethash "message" error-obj))))

;;; ============================================================================
;;; List Models
;;; ============================================================================

(defmethod list-models ((provider gemini-provider) &key (filter-generative t))
  "List available Gemini models.
If FILTER-GENERATIVE is T (default), only return models that support generateContent."
  (let* ((api-key (provider-api-key provider))
         (url (format nil "~A/models?key=~A"
                      (provider-endpoint provider)
                      api-key)))
    (unless api-key
      (error 'authentication-error
             :provider provider
             :message "No API key configured for Gemini"))
    (multiple-value-bind (response status)
        (http-get-json url)
      (if (and status (= status 200) response)
          (let ((models (gethash "models" response)))
            (loop for model in models
                  for name = (gethash "name" model)
                  for methods = (gethash "supportedGenerationMethods" model)
                  ;; Filter to only generative models if requested
                  when (or (not filter-generative)
                           (member "generateContent" methods :test #'string=))
                  collect (list :name (if (str:starts-with-p "models/" name)
                                          (subseq name 7)
                                          name)
                                :display-name (gethash "displayName" model)
                                :description (gethash "description" model)
                                :input-token-limit (gethash "inputTokenLimit" model)
                                :output-token-limit (gethash "outputTokenLimit" model)
                                :supported-methods methods)))
          (error 'provider-error
                 :provider provider
                 :status-code status
                 :message "Failed to list models")))))

(defun print-gemini-models (provider &key (stream *standard-output*))
  "Pretty-print available Gemini models to STREAM."
  (let ((models (list-models provider)))
    (format stream "~&Available Gemini models:~%")
    (format stream "~&~60A ~10A ~10A~%" "Model Name" "Input" "Output")
    (format stream "~&~60,,,'-A~%" "")
    (dolist (model models)
      (format stream "~&~60A ~10A ~10A~%"
              (getf model :name)
              (getf model :input-token-limit)
              (getf model :output-token-limit)))
    (length models)))
