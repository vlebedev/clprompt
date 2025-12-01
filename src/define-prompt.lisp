;;;; define-prompt.lisp - Macro-based DSL for defining prompts programmatically

(in-package #:clprompt)

;;; ============================================================================
;;; define-prompt Macro
;;; ============================================================================
;;;
;;; The define-prompt macro provides a DSL for creating prompts without .prompt
;;; files. It compiles the template at macro-expansion time for efficiency.
;;;
;;; Usage:
;;;   (define-prompt greeting-prompt
;;;     :model "googleai/gemini-1.5-pro"
;;;     :config (:temperature 0.7 :max-tokens 100)
;;;     :input (:name string
;;;             :formal? boolean?)
;;;     :output (:greeting string)
;;;     :template "Hello {{name}}! {{#if formal}}How may I assist you today?{{/if}}")
;;;
;;; This creates a variable GREETING-PROMPT bound to a DOTPROMPT instance.

(defmacro define-prompt (name &key model config input output template
                                   (output-format :text) metadata)
  "Define a prompt as a named variable.

NAME is a symbol that will be bound to the resulting DOTPROMPT instance.

Keyword arguments:
  :MODEL         - Model specification string (e.g., \"googleai/gemini-1.5-pro\")
  :CONFIG        - Plist of configuration (:temperature, :max-tokens, etc)
  :INPUT         - Input schema specification (plist of field-name type pairs)
  :OUTPUT        - Output schema specification (plist of field-name type pairs)
  :TEMPLATE      - Handlebars template string (required)
  :OUTPUT-FORMAT - Expected output format (:text, :json, etc). Default :text
  :METADATA      - Additional metadata plist

The template is compiled at macro-expansion time for efficiency.

Example:
  (define-prompt my-prompt
    :model \"googleai/gemini-1.5-pro\"
    :config (:temperature 0.7)
    :input (:name string :age integer?)
    :template \"Hello {{name}}, you are {{age}} years old.\")

  ;; Use the prompt:
  (render-prompt my-prompt '(:name \"Alice\" :age 30))"
  (check-type name symbol)
  (unless template
    (error "TEMPLATE is required for DEFINE-PROMPT"))
  `(defparameter ,name
     (make-dotprompt
      :model ,model
      :config ',config
      :input-schema ,(when input `(parse-schema-spec ',input))
      :output-schema ,(when output `(parse-schema-spec ',output))
      :output-format ,output-format
      :template (compile-template (make-handlebars-template ,template))
      :metadata ',metadata)))

;;; ============================================================================
;;; Schema Specification Parsing
;;; ============================================================================

(defun parse-schema-spec (spec)
  "Parse a schema specification plist into a schema object.

SPEC is a plist where keys are field names (symbols) and values are type
specifiers. Field names ending in ? indicate optional fields.

Type specifiers can be:
  STRING   - String value
  NUMBER   - Numeric value
  INTEGER  - Integer value
  BOOLEAN  - Boolean value
  ARRAY    - Array/list value
  (ARRAY type) - Array of specific type
  OBJECT   - Object/hash-table
  (OBJECT props...) - Object with specific properties
  (ENUM val1 val2 ...) - Enumeration of allowed values
  ANY      - Any value

Examples:
  (:name string :age integer? :tags (array string))
  (:status (enum :pending :active :done))
  (:user (object :name string :email string?))"
  (when spec
    (let ((properties nil))
      (loop for (field-name type-spec) on spec by #'cddr
            do (let* ((name-str (symbol-name field-name))
                      (optional-p (char= #\? (char name-str (1- (length name-str)))))
                      (clean-name (if optional-p
                                      (subseq name-str 0 (1- (length name-str)))
                                      name-str)))
                 (push (cons (string-downcase clean-name)
                             (parse-type-specifier type-spec (not optional-p)))
                       properties)))
      (make-object-schema :properties (nreverse properties)))))

(defun parse-type-specifier (spec required-p)
  "Parse a type specifier into a schema object.

SPEC can be:
  - A symbol: STRING, NUMBER, INTEGER, BOOLEAN, ARRAY, OBJECT, ANY
  - A list: (ARRAY item-type), (OBJECT props...), (ENUM values...)"
  (etypecase spec
    (symbol
     (let ((type-name (string-upcase (symbol-name spec))))
       (cond
         ((string= type-name "STRING")
          (make-string-schema :required-p required-p))
         ((string= type-name "NUMBER")
          (make-number-schema :required-p required-p))
         ((string= type-name "INTEGER")
          (make-number-schema :required-p required-p :integer-p t))
         ((string= type-name "BOOLEAN")
          (make-boolean-schema :required-p required-p))
         ((string= type-name "ARRAY")
          (make-array-schema :required-p required-p))
         ((string= type-name "OBJECT")
          (make-object-schema :required-p required-p))
         ((string= type-name "ANY")
          (make-instance 'any-schema :required-p required-p))
         (t
          ;; Handle type names that might end with ? (optional marker)
          (let ((optional-marker-p (char= #\? (char type-name (1- (length type-name))))))
            (if optional-marker-p
                (parse-type-specifier (intern (subseq type-name 0 (1- (length type-name)))
                                              (symbol-package spec))
                                      nil)
                (error "Unknown type specifier: ~A" spec)))))))
    (list
     (let ((type-name (string-upcase (symbol-name (first spec)))))
       (cond
         ;; (ARRAY item-type)
         ((string= type-name "ARRAY")
          (make-array-schema :required-p required-p
                             :items (when (second spec)
                                      (parse-type-specifier (second spec) t))))
         ;; (OBJECT props...)
         ((string= type-name "OBJECT")
          (let ((nested-props (rest spec)))
            (if nested-props
                (parse-schema-spec nested-props)
                (make-object-schema :required-p required-p))))
         ;; (ENUM val1 val2 ...)
         ((string= type-name "ENUM")
          (make-enum-schema :required-p required-p
                            :values (mapcar (lambda (v)
                                              (if (keywordp v)
                                                  (string-downcase (symbol-name v))
                                                  v))
                                            (rest spec))))
         (t
          (error "Unknown compound type specifier: ~A" spec)))))))

;;; ============================================================================
;;; Convenience Macros
;;; ============================================================================

(defmacro with-prompt ((var &rest prompt-args) &body body)
  "Execute BODY with VAR bound to a temporary prompt.

This is useful for one-off prompts that don't need to be named.

Example:
  (with-prompt (p :model \"gemini-1.5-pro\"
                  :template \"Summarize: {{text}}\")
    (execute p '(:text \"Long text here...\")))"
  (let ((processed-args nil))
    (loop for (key val) on prompt-args by #'cddr
          do (case key
               (:input (push :input-schema processed-args)
                       (push `(parse-schema-spec ',val) processed-args))
               (:output (push :output-schema processed-args)
                        (push `(parse-schema-spec ',val) processed-args))
               (:config (push :config processed-args)
                        (push `',val processed-args))
               (:metadata (push :metadata processed-args)
                          (push `',val processed-args))
               (:template (push :template processed-args)
                          (push `(compile-template (make-handlebars-template ,val))
                                processed-args))
               (otherwise (push key processed-args)
                          (push val processed-args))))
    `(let ((,var (make-dotprompt ,@(nreverse processed-args))))
       ,@body)))
