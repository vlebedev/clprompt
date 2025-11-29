;;;; tests/schema-tests.lisp - Schema validation tests

(in-package #:clprompt/tests)

(in-suite schema-tests)

;;; ============================================================================
;;; String Schema Tests
;;; ============================================================================

(test validate-string-valid
  "Valid string passes validation."
  (let ((schema (clprompt::make-string-schema :required-p t)))
    (is (string= "hello" (clprompt:validate schema "hello")))))

(test validate-string-invalid-type
  "Non-string value signals type error."
  (let ((schema (clprompt::make-string-schema :required-p t)))
    (signals clprompt:type-validation-error
      (clprompt:validate schema 123))))

(test validate-string-min-length
  "String shorter than min-length signals error."
  (let ((schema (clprompt::make-string-schema :min-length 5)))
    (signals clprompt:validation-error
      (clprompt:validate schema "hi"))))

(test validate-string-max-length
  "String longer than max-length signals error."
  (let ((schema (clprompt::make-string-schema :max-length 3)))
    (signals clprompt:validation-error
      (clprompt:validate schema "hello"))))

;;; ============================================================================
;;; Number Schema Tests
;;; ============================================================================

(test validate-number-valid
  "Valid number passes validation."
  (let ((schema (clprompt::make-number-schema)))
    (is (= 42 (clprompt:validate schema 42)))
    (is (= 3.14 (clprompt:validate schema 3.14)))))

(test validate-number-invalid-type
  "Non-number value signals type error."
  (let ((schema (clprompt::make-number-schema)))
    (signals clprompt:type-validation-error
      (clprompt:validate schema "42"))))

(test validate-integer-rejects-float
  "Integer schema rejects floating point."
  (let ((schema (clprompt::make-number-schema :integer-p t)))
    (signals clprompt:type-validation-error
      (clprompt:validate schema 3.14))))

(test validate-number-range
  "Number outside range signals error."
  (let ((schema (clprompt::make-number-schema :minimum 0 :maximum 100)))
    (is (= 50 (clprompt:validate schema 50)))
    (signals clprompt:validation-error
      (clprompt:validate schema -1))
    (signals clprompt:validation-error
      (clprompt:validate schema 101))))

;;; ============================================================================
;;; Boolean Schema Tests
;;; ============================================================================

(test validate-boolean-valid
  "Valid booleans pass validation."
  (let ((schema (clprompt::make-boolean-schema)))
    (is (eq t (clprompt:validate schema t)))
    (is (eq nil (clprompt:validate schema nil)))))

(test validate-boolean-invalid-type
  "Non-boolean value signals type error."
  (let ((schema (clprompt::make-boolean-schema)))
    (signals clprompt:type-validation-error
      (clprompt:validate schema "true"))))

;;; ============================================================================
;;; Array Schema Tests
;;; ============================================================================

(test validate-array-valid
  "Valid array passes validation."
  (let ((schema (clprompt::make-array-schema)))
    (is (equal '(1 2 3) (clprompt:validate schema '(1 2 3))))))

(test validate-array-invalid-type
  "Non-list value signals type error."
  (let ((schema (clprompt::make-array-schema)))
    (signals clprompt:type-validation-error
      (clprompt:validate schema "not a list"))))

(test validate-array-item-schema
  "Array items are validated against item schema."
  (let ((schema (clprompt::make-array-schema
                 :items (clprompt::make-string-schema))))
    (is (equal '("a" "b") (clprompt:validate schema '("a" "b"))))
    (signals clprompt:type-validation-error
      (clprompt:validate schema '("a" 123)))))

;;; ============================================================================
;;; Object Schema Tests
;;; ============================================================================

(test validate-object-valid
  "Valid object passes validation."
  (let ((schema (clprompt::make-object-schema
                 :properties (list (cons "name" (clprompt::make-string-schema))))))
    (is (clprompt:validate schema '(:name "Alice")))))

(test validate-object-missing-required
  "Missing required field signals error."
  (let ((schema (clprompt::make-object-schema
                 :properties (list (cons "name"
                                         (clprompt::make-string-schema
                                          :required-p t))))))
    (signals clprompt:missing-required-error
      (clprompt:validate schema '()))))

(test validate-object-optional-field
  "Missing optional field is allowed."
  (let ((schema (clprompt::make-object-schema
                 :properties (list (cons "name"
                                         (clprompt::make-string-schema
                                          :required-p nil))))))
    ;; Validate should not signal an error and return the value
    (finishes (clprompt:validate schema '()))))

;;; ============================================================================
;;; Enum Schema Tests
;;; ============================================================================

(test validate-enum-valid
  "Value in enum list passes validation."
  (let ((schema (clprompt::make-enum-schema :values '("red" "green" "blue"))))
    (is (string= "red" (clprompt:validate schema "red")))))

(test validate-enum-invalid
  "Value not in enum list signals error."
  (let ((schema (clprompt::make-enum-schema :values '("red" "green" "blue"))))
    (signals clprompt:validation-error
      (clprompt:validate schema "yellow"))))
