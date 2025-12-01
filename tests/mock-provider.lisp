;;;; tests/mock-provider.lisp - Mock LLM provider for testing

(in-package #:clprompt/tests)

;;; ============================================================================
;;; Mock Provider Class
;;; ============================================================================

(defclass mock-provider (clprompt:provider)
  ((name :initform "mock"
         :reader clprompt:provider-name)
   (responses :initarg :responses
              :initform nil
              :accessor mock-responses
              :documentation "Queue of canned responses to return.")
   (requests :initform nil
             :accessor mock-requests
             :documentation "List of requests received (for assertions)."))
  (:documentation "Mock provider that returns canned responses for testing."))

(defmethod clprompt:provider-available-p ((provider mock-provider))
  t)

(defmethod clprompt:provider-api-key ((provider mock-provider))
  "mock-api-key")

(defmethod clprompt:provider-model ((provider mock-provider))
  "mock-model")

(defmethod clprompt:provider-endpoint ((provider mock-provider))
  "http://mock.example.com")

(defmethod clprompt:send-request ((provider mock-provider) prompt &key
                                   model temperature max-tokens output-schema)
  "Return the next canned response from the queue."
  (declare (ignore model temperature max-tokens output-schema))
  ;; Record the request
  (push prompt (mock-requests provider))
  ;; Return canned response
  (let ((responses (mock-responses provider)))
    (if responses
        (let ((response (pop (mock-responses provider))))
          (if (functionp response)
              (funcall response prompt)
              response))
        ;; Default response if queue is empty
        (list :content "Mock response"
              :usage (list :prompt-tokens 10
                          :completion-tokens 5
                          :total-tokens 15)))))

(defmethod clprompt:send-request-streaming ((provider mock-provider) prompt &key on-token
                                            model temperature max-tokens output-schema
                                            &allow-other-keys)
  "Stream canned responses for testing. RESPONSES may contain lists of chunks or strings."
  (declare (ignore model temperature max-tokens output-schema))
  (push prompt (mock-requests provider))
  (let* ((responses (mock-responses provider))
         (next (when responses (pop responses))))
    (setf (mock-responses provider) responses)
    (let ((chunks (cond
                    ((functionp next) (funcall next prompt))
                    ((and (listp next) (every #'stringp next)) next)
                    ((stringp next) (list next))
                    (t (list "Mock streamed response")))))
      (dolist (chunk chunks)
        (when on-token (funcall on-token chunk)))
      (list :content (apply #'concatenate 'string chunks)
            :raw chunks
            :finish-reason :done))))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun make-mock-provider (&rest responses)
  "Create a mock provider with the given RESPONSES queue."
  (make-instance 'mock-provider :responses responses))

(defun make-json-response (data)
  "Create a mock response with JSON content from DATA.
DATA can be a hash-table or an alist (which will be converted to hash-table)."
  (let ((ht (etypecase data
              (hash-table data)
              (list (alexandria:alist-hash-table data :test 'equal)))))
    (list :content (with-output-to-string (s) (yason:encode ht s))
          :usage (list :prompt-tokens 10 :completion-tokens 5 :total-tokens 15))))

(defun last-request (provider)
  "Get the most recent request sent to PROVIDER."
  (first (mock-requests provider)))

(defun request-count (provider)
  "Get the number of requests sent to PROVIDER."
  (length (mock-requests provider)))
