;;;; model.lisp - Core domain classes for clprompt

(in-package #:clprompt)

;;; ============================================================================
;;; Dotprompt Class
;;; ============================================================================

(defclass dotprompt ()
  ((model :initarg :model
          :initform nil
          :reader prompt-model
          :documentation "Model specification (e.g., \"googleai/gemini-1.5-pro\").")
   (config :initarg :config
           :initform nil
           :reader prompt-config
           :documentation "Configuration plist (:temperature, :max-tokens, etc).")
   (input-schema :initarg :input-schema
                 :initform nil
                 :reader prompt-input-schema
                 :documentation "Schema object for validating input.")
   (output-schema :initarg :output-schema
                  :initform nil
                  :reader prompt-output-schema
                  :documentation "Schema object for parsing/validating output.")
   (output-format :initarg :output-format
                  :initform :text
                  :reader prompt-output-format
                  :documentation "Expected output format (:json, :text, etc).")
   (template :initarg :template
             :initform (error "TEMPLATE is required")
             :reader prompt-template
             :documentation "The Handlebars template object.")
   (metadata :initarg :metadata
             :initform nil
             :reader prompt-metadata
             :documentation "Additional frontmatter metadata."))
  (:documentation "Represents a parsed dotprompt file with its configuration and template."))

(defmethod print-object ((prompt dotprompt) stream)
  (print-unreadable-object (prompt stream :type t :identity t)
    (format stream "~@[model: ~A~]" (prompt-model prompt))))

;;; ============================================================================
;;; Dotprompt Construction
;;; ============================================================================

(defun make-dotprompt (&key model config input-schema output-schema
                            output-format template metadata)
  "Construct a new DOTPROMPT instance."
  (make-instance 'dotprompt
                 :model model
                 :config config
                 :input-schema input-schema
                 :output-schema output-schema
                 :output-format (or output-format :text)
                 :template template
                 :metadata metadata))

;;; ============================================================================
;;; Config Accessors
;;; ============================================================================

(defun prompt-temperature (prompt)
  "Return the temperature setting from PROMPT's config, or NIL."
  (getf (prompt-config prompt) :temperature))

(defun prompt-max-tokens (prompt)
  "Return the max-tokens setting from PROMPT's config, or NIL."
  (getf (prompt-config prompt) :max-tokens))

(defun prompt-top-p (prompt)
  "Return the top-p setting from PROMPT's config, or NIL."
  (getf (prompt-config prompt) :top-p))

(defun prompt-top-k (prompt)
  "Return the top-k setting from PROMPT's config, or NIL."
  (getf (prompt-config prompt) :top-k))

(defun prompt-stop-sequences (prompt)
  "Return the stop sequences from PROMPT's config, or NIL."
  (getf (prompt-config prompt) :stop-sequences))
