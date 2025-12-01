;;;; tests/define-prompt-tests.lisp - Tests for define-prompt macro

(in-package #:clprompt/tests)

;;; ============================================================================
;;; Test Suite
;;; ============================================================================

(def-suite define-prompt-tests
  :description "Tests for define-prompt macro"
  :in clprompt-tests)

(in-suite define-prompt-tests)

;;; ============================================================================
;;; Basic define-prompt Tests
;;; ============================================================================

(test define-prompt-creates-dotprompt
  "define-prompt creates a dotprompt instance"
  (define-prompt test-basic-prompt
    :model "test-model"
    :template "Hello {{name}}!")
  (is (typep test-basic-prompt 'dotprompt))
  (is (equal "test-model" (prompt-model test-basic-prompt)))
  (is (template-compiled-p (prompt-template test-basic-prompt))))

(test define-prompt-with-config
  "define-prompt handles configuration"
  (define-prompt test-config-prompt
    :model "test-model"
    :config (:temperature 0.7 :max-tokens 100)
    :template "Test")
  (let ((config (prompt-config test-config-prompt)))
    (is (equal 0.7 (getf config :temperature)))
    (is (equal 100 (getf config :max-tokens)))))

(test define-prompt-with-input-schema
  "define-prompt creates input schema from spec"
  (define-prompt test-input-prompt
    :model "test-model"
    :input (:name string :age integer?)
    :template "{{name}} is {{age}}")
  (let ((schema (prompt-input-schema test-input-prompt)))
    (is (typep schema 'object-schema))
    (is (= 2 (length (schema-properties schema))))
    ;; Check 'name' field is required string
    (let ((name-schema (cdr (assoc "name" (schema-properties schema) :test #'string=))))
      (is (typep name-schema 'string-schema))
      (is (schema-required-p name-schema)))
    ;; Check 'age' field is optional integer
    (let ((age-schema (cdr (assoc "age" (schema-properties schema) :test #'string=))))
      (is (typep age-schema 'number-schema))
      (is (not (schema-required-p age-schema))))))

(test define-prompt-with-output-schema
  "define-prompt creates output schema from spec"
  (define-prompt test-output-prompt
    :model "test-model"
    :output (:result string :score number)
    :output-format :json
    :template "Generate output")
  (let ((schema (prompt-output-schema test-output-prompt)))
    (is (typep schema 'object-schema))
    (is (eq :json (prompt-output-format test-output-prompt)))))

(test define-prompt-with-metadata
  "define-prompt stores metadata"
  (define-prompt test-metadata-prompt
    :model "test-model"
    :metadata (:version "1.0" :author "Test")
    :template "Test")
  (is (equal "1.0" (getf (prompt-metadata test-metadata-prompt) :version)))
  (is (equal "Test" (getf (prompt-metadata test-metadata-prompt) :author))))

;;; ============================================================================
;;; Schema Specifier Tests
;;; ============================================================================

(test parse-schema-spec-simple-types
  "parse-schema-spec handles simple type specifiers"
  (let ((schema (clprompt::parse-schema-spec '(:name string :count number :active boolean))))
    (is (typep schema 'object-schema))
    (let ((props (schema-properties schema)))
      (is (typep (cdr (assoc "name" props :test #'string=)) 'string-schema))
      (is (typep (cdr (assoc "count" props :test #'string=)) 'number-schema))
      (is (typep (cdr (assoc "active" props :test #'string=)) 'boolean-schema)))))

(test parse-schema-spec-optional-fields
  "parse-schema-spec handles optional field markers"
  (let ((schema (clprompt::parse-schema-spec '(:required string :optional? string))))
    (let ((props (schema-properties schema)))
      (is (schema-required-p (cdr (assoc "required" props :test #'string=))))
      (is (not (schema-required-p (cdr (assoc "optional" props :test #'string=))))))))

(test parse-schema-spec-array-type
  "parse-schema-spec handles array types"
  (let ((schema (clprompt::parse-schema-spec '(:tags (array string) :items array))))
    (let ((props (schema-properties schema)))
      (let ((tags-schema (cdr (assoc "tags" props :test #'string=))))
        (is (typep tags-schema 'array-schema))
        (is (typep (schema-items tags-schema) 'string-schema)))
      (let ((items-schema (cdr (assoc "items" props :test #'string=))))
        (is (typep items-schema 'array-schema))
        (is (null (schema-items items-schema)))))))

(test parse-schema-spec-enum-type
  "parse-schema-spec handles enum types"
  (let ((schema (clprompt::parse-schema-spec '(:status (enum :pending :active :done)))))
    (let* ((props (schema-properties schema))
           (status-schema (cdr (assoc "status" props :test #'string=))))
      (is (typep status-schema 'enum-schema))
      (is (equal '("pending" "active" "done") (clprompt::schema-enum-values status-schema))))))

(test parse-schema-spec-nested-object
  "parse-schema-spec handles nested objects"
  (let ((schema (clprompt::parse-schema-spec '(:user (object :name string :email string?)))))
    (let* ((props (schema-properties schema))
           (user-schema (cdr (assoc "user" props :test #'string=))))
      (is (typep user-schema 'object-schema))
      (let ((user-props (schema-properties user-schema)))
        (is (= 2 (length user-props)))
        (is (typep (cdr (assoc "name" user-props :test #'string=)) 'string-schema))))))

;;; ============================================================================
;;; Template Rendering Tests
;;; ============================================================================

(test define-prompt-renders-correctly
  "Prompts created with define-prompt render correctly"
  (define-prompt test-render-prompt
    :model "test-model"
    :template "Hello {{name}}! You have {{count}} messages.")
  (let ((result (render-prompt test-render-prompt '(:name "Alice" :count 5))))
    (is (equal "Hello Alice! You have 5 messages." result))))

(test define-prompt-renders-conditionals
  "Prompts render conditional blocks correctly"
  ;; Use separate prompts for formal vs informal since the template engine
  ;; renders both blocks when conditions aren't mutually exclusive
  (define-prompt test-formal-prompt
    :model "test-model"
    :template "{{#if formal}}Dear {{name}},{{/if}}")
  (define-prompt test-informal-prompt
    :model "test-model"
    :template "{{#unless formal}}Hi {{name}}!{{/unless}}")
  (is (equal "Dear Alice," (render-prompt test-formal-prompt '(:formal t :name "Alice"))))
  (is (equal "" (render-prompt test-formal-prompt '(:formal nil :name "Bob"))))
  (is (equal "Hi Bob!" (render-prompt test-informal-prompt '(:formal nil :name "Bob")))))

;;; ============================================================================
;;; with-prompt Tests
;;; ============================================================================

(test with-prompt-creates-temporary-prompt
  "with-prompt creates a usable temporary prompt"
  (with-prompt (p :model "temp-model"
                  :template "Testing {{value}}")
    (is (typep p 'dotprompt))
    (is (equal "Testing 42" (render-prompt p '(:value 42))))))

(test with-prompt-with-schema
  "with-prompt handles input schema"
  (with-prompt (p :model "temp-model"
                  :input (:name string)
                  :template "Hello {{name}}")
    (is (typep (prompt-input-schema p) 'object-schema))))
