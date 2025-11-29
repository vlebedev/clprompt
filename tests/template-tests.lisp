;;;; tests/template-tests.lisp - Template engine tests

(in-package #:clprompt/tests)

(in-suite template-tests)

;;; ============================================================================
;;; Basic Variable Substitution
;;; ============================================================================

(test render-simple-variable
  "Simple variable substitution works."
  (let ((template (clprompt::make-handlebars-template "Hello {{name}}!")))
    (is (string= "Hello World!"
                 (clprompt:render template '(:name "World"))))))

(test render-multiple-variables
  "Multiple variables are substituted correctly."
  (let ((template (clprompt::make-handlebars-template
                   "{{greeting}}, {{name}}!")))
    (is (string= "Hi, Alice!"
                 (clprompt:render template '(:greeting "Hi" :name "Alice"))))))

(test render-missing-variable
  "Missing variables render as empty strings."
  (let ((template (clprompt::make-handlebars-template "Hello {{name}}!")))
    (is (string= "Hello !"
                 (clprompt:render template '())))))

;;; ============================================================================
;;; Dotted Path Access
;;; ============================================================================

(test render-dotted-path
  "Dotted path variable access works."
  (let ((template (clprompt::make-handlebars-template
                   "Name: {{person.name}}")))
    (is (string= "Name: John"
                 (clprompt:render template
                                  (alexandria:plist-hash-table
                                   (list "person"
                                         (alexandria:plist-hash-table
                                          '("name" "John")
                                          :test 'equal))
                                   :test 'equal))))))

;;; ============================================================================
;;; Block Expressions
;;; ============================================================================

(test render-if-block-true
  "#if block renders when condition is truthy."
  (let ((template (clprompt::make-handlebars-template
                   "{{#if show}}Visible{{/if}}")))
    (is (string= "Visible"
                 (clprompt:render template '(:show t))))))

(test render-if-block-false
  "#if block does not render when condition is falsy."
  (let ((template (clprompt::make-handlebars-template
                   "{{#if show}}Visible{{/if}}")))
    (is (string= ""
                 (clprompt:render template '(:show nil))))))

(test render-unless-block
  "#unless block renders when condition is falsy."
  (let ((template (clprompt::make-handlebars-template
                   "{{#unless hidden}}Visible{{/unless}}")))
    (is (string= "Visible"
                 (clprompt:render template '(:hidden nil))))))

(test render-each-block
  "#each block iterates over list."
  (let ((template (clprompt::make-handlebars-template
                   "{{#each items}}{{this}},{{/each}}")))
    (is (string= "a,b,c,"
                 (clprompt:render template '(:items ("a" "b" "c")))))))

;;; ============================================================================
;;; Comments
;;; ============================================================================

(test render-comment-removed
  "Comments are removed from output."
  (let ((template (clprompt::make-handlebars-template
                   "Hello{{! this is a comment }} World")))
    (is (string= "Hello World"
                 (clprompt:render template '())))))

;;; ============================================================================
;;; Template Compilation
;;; ============================================================================

(test template-compilation-idempotent
  "Compiling a template twice is idempotent."
  (let ((template (clprompt::make-handlebars-template "Test {{var}}")))
    (clprompt:compile-template template)
    (is (clprompt:template-compiled-p template))
    (let ((compiled-1 (clprompt::%template-compiled template)))
      (clprompt:compile-template template)
      (is (eq compiled-1 (clprompt::%template-compiled template))))))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(test render-json-helper-string
  "{{json var}} serializes a string to JSON."
  (let ((template (clprompt::make-handlebars-template "Data: {{json name}}")))
    (is (string= "Data: \"Alice\""
                 (clprompt:render template '(:name "Alice"))))))

(test render-json-helper-number
  "{{json var}} serializes a number to JSON."
  (let ((template (clprompt::make-handlebars-template "Count: {{json count}}")))
    (is (string= "Count: 42"
                 (clprompt:render template '(:count 42))))))

(test render-json-helper-object
  "{{json var}} serializes an object to JSON."
  (let ((template (clprompt::make-handlebars-template "User: {{json user}}")))
    (is (string= "User: {\"name\":\"Bob\",\"age\":30}"
                 (clprompt:render template
                                  (alexandria:plist-hash-table
                                   (list "user"
                                         (alexandria:plist-hash-table
                                          '("name" "Bob" "age" 30)
                                          :test 'equal))
                                   :test 'equal))))))

(test render-json-helper-array
  "{{json var}} serializes an array to JSON."
  (let ((template (clprompt::make-handlebars-template "Items: {{json items}}")))
    (is (string= "Items: [1,2,3]"
                 (clprompt:render template '(:items (1 2 3)))))))
