;;;; providers/openai.lisp - OpenAI provider implementation

(in-package #:clprompt)

;;; ============================================================================
;;; OpenAI Provider Class
;;; ============================================================================

(defclass openai-provider (provider api-key-mixin)
  ((model :initarg :model
          :initform "gpt-4"
          :reader provider-model
          :documentation "OpenAI model identifier.")
   (endpoint :initarg :endpoint
             :initform "https://api.openai.com/v1"
             :reader provider-endpoint
             :documentation "OpenAI API base endpoint."))
  (:documentation "OpenAI API provider."))

(defmethod provider-name ((provider openai-provider))
  "openai")

(defmethod provider-env-var-name ((provider openai-provider))
  "OPENAI_API_KEY")

;;; ============================================================================
;;; OpenAI API Implementation
;;; ============================================================================

(defmethod send-request ((provider openai-provider) prompt &key
                         model temperature max-tokens output-schema)
  "Send a prompt to OpenAI and return the response."
  (let* ((actual-model (or model (provider-model provider)))
         (api-key (provider-api-key provider))
         (url (format nil "~A/chat/completions" (provider-endpoint provider))))
    (unless api-key
      (error 'authentication-error
             :provider provider
             :message "No API key configured for OpenAI"))
    (let ((body (build-openai-request prompt actual-model
                                      :temperature temperature
                                      :max-tokens max-tokens
                                      :output-schema output-schema))
          (headers (list (build-auth-header api-key :type :bearer))))
      (multiple-value-bind (response status resp-headers error-body)
          (http-post-json url body :headers headers)
        (declare (ignore resp-headers))
        (handle-openai-response provider response status error-body)))))

(defun build-openai-request (prompt model &key temperature max-tokens output-schema)
  "Build the request body for OpenAI API."
  (let ((request (make-hash-table :test 'equal)))
    (setf (gethash "model" request) model)
    (setf (gethash "messages" request)
          (list (let ((msg (make-hash-table :test 'equal)))
                  (setf (gethash "role" msg) "user")
                  (setf (gethash "content" msg) prompt)
                  msg)))
    (when temperature
      (setf (gethash "temperature" request) temperature))
    (when max-tokens
      (setf (gethash "max_tokens" request) max-tokens))
    (when output-schema
      (setf (gethash "response_format" request)
            (let ((fmt (make-hash-table :test 'equal)))
              (setf (gethash "type" fmt) "json_object")
              fmt)))
    request))

(defun handle-openai-response (provider response status error-body)
  "Handle the OpenAI API response."
  (cond
    ;; Success
    ((and status (= status 200) response)
     (parse-openai-response response))
    ;; Rate limit
    ((and status (= status 429))
     (error 'rate-limit-error
            :provider provider
            :status-code status
            :message "Rate limit exceeded"))
    ;; Auth error
    ((and status (= status 401))
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
            :message (format nil "OpenAI API error: ~A"
                             (or (extract-openai-error response)
                                 error-body
                                 "Unknown error"))))))

(defun parse-openai-response (response)
  "Parse successful OpenAI response into standardized format."
  (let* ((choices (gethash "choices" response))
         (first-choice (and choices (first choices)))
         (message (and first-choice (gethash "message" first-choice)))
         (content (and message (gethash "content" message)))
         (usage (gethash "usage" response))
         (finish-reason (and first-choice (gethash "finish_reason" first-choice))))
    (list :content content
          :raw response
          :finish-reason finish-reason
          :usage (when usage
                   (list :prompt-tokens (gethash "prompt_tokens" usage)
                         :completion-tokens (gethash "completion_tokens" usage)
                         :total-tokens (gethash "total_tokens" usage))))))

(defun extract-openai-error (response)
  "Extract error message from OpenAI error response."
  (when (hash-table-p response)
    (when-let (error-obj (gethash "error" response))
      (gethash "message" error-obj))))

;;; ============================================================================
;;; List Models
;;; ============================================================================

(defmethod list-models ((provider openai-provider) &key)
  "List available OpenAI models."
  (let* ((api-key (provider-api-key provider))
         (url (format nil "~A/models" (provider-endpoint provider)))
         (headers (list (build-auth-header api-key :type :bearer))))
    (unless api-key
      (error 'authentication-error
             :provider provider
             :message "No API key configured for OpenAI"))
    (multiple-value-bind (response status)
        (http-get-json url :headers headers)
      (if (and status (= status 200) response)
          (let ((models (gethash "data" response)))
            (sort
             (loop for model in models
                   for id = (gethash "id" model)
                   ;; Filter to chat models (gpt-*)
                   when (str:starts-with-p "gpt-" id)
                   collect (list :name id
                                 :owned-by (gethash "owned_by" model)
                                 :created (gethash "created" model)))
             #'string<
             :key (lambda (m) (getf m :name))))
          (error 'provider-error
                 :provider provider
                 :status-code status
                 :message "Failed to list models")))))

(defun print-openai-models (provider &key (stream *standard-output*))
  "Pretty-print available OpenAI models to STREAM."
  (let ((models (list-models provider)))
    (format stream "~&Available OpenAI models:~%")
    (format stream "~&~40A ~20A~%" "Model Name" "Owned By")
    (format stream "~&~40,,,'-A~%" "")
    (dolist (model models)
      (format stream "~&~40A ~20A~%"
              (getf model :name)
              (getf model :owned-by)))
    (length models)))
