;;;; conditions.lisp - Condition hierarchy for clprompt

(in-package #:clprompt)

;;; ============================================================================
;;; Base Condition
;;; ============================================================================

(define-condition clprompt-error (error)
  ((message :initarg :message
            :initform nil
            :reader error-message))
  (:documentation "Base condition for all clprompt errors.")
  (:report (lambda (condition stream)
             (format stream "clprompt error: ~A"
                     (or (error-message condition) "Unknown error")))))

;;; ============================================================================
;;; Parse Errors
;;; ============================================================================

(define-condition prompt-parse-error (clprompt-error)
  ((line :initarg :line
         :initform nil
         :reader error-line)
   (column :initarg :column
           :initform nil
           :reader error-column)
   (source :initarg :source
           :initform nil
           :reader error-source))
  (:documentation "Signaled when parsing a .prompt file fails.")
  (:report (lambda (condition stream)
             (format stream "Parse error~@[ at line ~A~]~@[, column ~A~]: ~A"
                     (error-line condition)
                     (error-column condition)
                     (or (error-message condition) "Unknown parse error")))))

(define-condition frontmatter-error (prompt-parse-error)
  ()
  (:documentation "Signaled when YAML frontmatter parsing fails.")
  (:report (lambda (condition stream)
             (format stream "Frontmatter error~@[ at line ~A~]: ~A"
                     (error-line condition)
                     (or (error-message condition) "Invalid YAML frontmatter")))))

(define-condition template-syntax-error (prompt-parse-error)
  ()
  (:documentation "Signaled when Handlebars template syntax is invalid.")
  (:report (lambda (condition stream)
             (format stream "Template syntax error~@[ at line ~A~]~@[, column ~A~]: ~A"
                     (error-line condition)
                     (error-column condition)
                     (or (error-message condition) "Invalid template syntax")))))

;;; ============================================================================
;;; Validation Errors
;;; ============================================================================

(define-condition validation-error (clprompt-error)
  ((schema :initarg :schema
           :initform nil
           :reader error-schema)
   (value :initarg :value
          :initform nil
          :reader error-value)
   (path :initarg :path
         :initform nil
         :reader error-path))
  (:documentation "Signaled when input validation against a schema fails.")
  (:report (lambda (condition stream)
             (format stream "Validation error~@[ at path ~{~A~^.~}~]: ~A (got ~S)"
                     (error-path condition)
                     (or (error-message condition) "Value does not match schema")
                     (error-value condition)))))

(define-condition type-validation-error (validation-error)
  ((expected-type :initarg :expected-type
                  :initform nil
                  :reader error-expected-type))
  (:documentation "Signaled when a value has an unexpected type.")
  (:report (lambda (condition stream)
             (format stream "Type error~@[ at path ~{~A~^.~}~]: expected ~A, got ~S"
                     (error-path condition)
                     (error-expected-type condition)
                     (error-value condition)))))

(define-condition missing-required-error (validation-error)
  ((field :initarg :field
          :initform nil
          :reader error-field))
  (:documentation "Signaled when a required field is missing.")
  (:report (lambda (condition stream)
             (format stream "Missing required field~@[ at path ~{~A~^.~}~]: ~A"
                     (error-path condition)
                     (error-field condition)))))

;;; ============================================================================
;;; Template Errors
;;; ============================================================================

(define-condition template-error (clprompt-error)
  ((template :initarg :template
             :initform nil
             :reader error-template))
  (:documentation "Signaled when template rendering fails.")
  (:report (lambda (condition stream)
             (format stream "Template error: ~A"
                     (or (error-message condition) "Template rendering failed")))))

(define-condition undefined-variable-error (template-error)
  ((variable :initarg :variable
             :initform nil
             :reader error-variable))
  (:documentation "Signaled when a template references an undefined variable.")
  (:report (lambda (condition stream)
             (format stream "Undefined variable in template: ~A"
                     (error-variable condition)))))

(define-condition undefined-helper-error (template-error)
  ((helper :initarg :helper
           :initform nil
           :reader error-helper))
  (:documentation "Signaled when a template references an undefined helper.")
  (:report (lambda (condition stream)
             (format stream "Undefined helper in template: ~A"
                     (error-helper condition)))))

;;; ============================================================================
;;; Schema Errors
;;; ============================================================================

(define-condition schema-error (clprompt-error)
  ((schema-source :initarg :schema-source
                  :initform nil
                  :reader error-schema-source))
  (:documentation "Signaled when schema definition is invalid.")
  (:report (lambda (condition stream)
             (format stream "Schema error: ~A"
                     (or (error-message condition) "Invalid schema definition")))))

;;; ============================================================================
;;; Provider Errors
;;; ============================================================================

(define-condition provider-error (clprompt-error)
  ((provider :initarg :provider
             :initform nil
             :reader error-provider)
   (status-code :initarg :status-code
                :initform nil
                :reader error-status-code)
   (response-body :initarg :response-body
                  :initform nil
                  :reader error-response-body))
  (:documentation "Signaled when an LLM provider request fails.")
  (:report (lambda (condition stream)
             (format stream "Provider error~@[ (~A)~]~@[ [HTTP ~A]~]: ~A"
                     (when-let (p (error-provider condition))
                       (provider-name p))
                     (error-status-code condition)
                     (or (error-message condition) "Provider request failed")))))

(define-condition authentication-error (provider-error)
  ()
  (:documentation "Signaled when provider authentication fails.")
  (:report (lambda (condition stream)
             (format stream "Authentication failed for provider~@[ ~A~]: ~A"
                     (when-let (p (error-provider condition))
                       (provider-name p))
                     (or (error-message condition) "Invalid or missing API key")))))

(define-condition rate-limit-error (provider-error)
  ((retry-after :initarg :retry-after
                :initform nil
                :reader error-retry-after))
  (:documentation "Signaled when provider rate limit is exceeded.")
  (:report (lambda (condition stream)
             (format stream "Rate limit exceeded~@[ for ~A~]~@[, retry after ~A seconds~]"
                     (when-let (p (error-provider condition))
                       (provider-name p))
                     (error-retry-after condition)))))

;;; ============================================================================
;;; Restarts
;;; ============================================================================

(defun invoke-use-default ()
  "Invoke the USE-DEFAULT restart if available."
  (when-let (restart (find-restart 'use-default))
    (invoke-restart restart)))

(defun invoke-skip-validation ()
  "Invoke the SKIP-VALIDATION restart if available."
  (when-let (restart (find-restart 'skip-validation))
    (invoke-restart restart)))

(defun invoke-provide-value (value)
  "Invoke the PROVIDE-VALUE restart with VALUE if available."
  (when-let (restart (find-restart 'provide-value))
    (invoke-restart restart value)))
