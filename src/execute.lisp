;;;; execute.lisp - Execution protocol implementation for clprompt

(in-package #:clprompt)

;;; ============================================================================
;;; Execution Protocol Implementation
;;; ============================================================================

(defmethod execute ((prompt dotprompt) input &key provider &allow-other-keys)
  "Execute PROMPT with INPUT, returning the LLM response.

Validates input, renders template, sends to provider, returns response.
When output-format is :json, parses response and validates against output-schema."
  ;; Validate input against schema
  (validate-input prompt input)
  ;; Render the template
  (let* ((rendered (render-prompt prompt input))
         ;; Use provided provider or fall back to default
         (actual-provider (or provider *default-provider*)))
    (unless actual-provider
      (error 'provider-error
             :message "No provider specified and *default-provider* is not set"))
    ;; Send to provider with config from prompt
    (let ((response (send-request actual-provider rendered
                                  :model (prompt-model prompt)
                                  :temperature (prompt-temperature prompt)
                                  :max-tokens (prompt-max-tokens prompt)
                                  :output-schema (prompt-output-schema prompt))))
      ;; Process response based on output format
      (process-response prompt response))))

(defun process-response (prompt response)
  "Process the LLM response based on prompt's output-format and output-schema.
Returns the response plist with :content potentially parsed and validated."
  (let ((content (getf response :content))
        (output-format (prompt-output-format prompt))
        (output-schema (prompt-output-schema prompt)))
    ;; Parse JSON if output-format is :json
    (when (and content (eq output-format :json))
      (let ((parsed (parse-json-response content)))
        ;; Validate against output schema if defined
        (when output-schema
          (validate output-schema parsed))
        ;; Replace content with parsed structure
        (setf (getf response :content) parsed)))
    response))

(defun parse-json-response (content)
  "Parse JSON content string, returning a hash-table or list.
Signals validation-error on parse failure."
  (handler-case
      (yason:parse content)
    (error (e)
      (error 'validation-error
             :message (format nil "Failed to parse JSON response: ~A" e)
             :value content))))

(defun execute-prompt (prompt input &rest keys &key provider &allow-other-keys)
  "Execute PROMPT with INPUT, returning the LLM response.

This is a convenience function that calls EXECUTE.
See EXECUTE for full documentation."
  (declare (ignore provider))
  (apply #'execute prompt input keys))
