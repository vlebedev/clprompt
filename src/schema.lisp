;;;; schema.lisp - Schema validation for clprompt

(in-package #:clprompt)

;;; ============================================================================
;;; Schema Classes
;;; ============================================================================

(defclass schema ()
  ((required-p :initarg :required-p
               :initform t
               :reader schema-required-p
               :documentation "Whether this field is required.")
   (description :initarg :description
                :initform nil
                :reader schema-description
                :documentation "Human-readable description."))
  (:documentation "Abstract base class for schemas."))

(defclass string-schema (schema)
  ((min-length :initarg :min-length
               :initform nil
               :reader schema-min-length)
   (max-length :initarg :max-length
               :initform nil
               :reader schema-max-length)
   (pattern :initarg :pattern
            :initform nil
            :reader schema-pattern))
  (:documentation "Schema for string values."))

(defclass number-schema (schema)
  ((minimum :initarg :minimum
            :initform nil
            :reader schema-minimum)
   (maximum :initarg :maximum
            :initform nil
            :reader schema-maximum)
   (integer-p :initarg :integer-p
              :initform nil
              :reader schema-integer-p))
  (:documentation "Schema for numeric values."))

(defclass boolean-schema (schema)
  ()
  (:documentation "Schema for boolean values."))

(defclass array-schema (schema)
  ((items :initarg :items
          :initform nil
          :reader schema-items
          :documentation "Schema for array items.")
   (min-items :initarg :min-items
              :initform nil
              :reader schema-min-items)
   (max-items :initarg :max-items
              :initform nil
              :reader schema-max-items))
  (:documentation "Schema for array values."))

(defclass object-schema (schema)
  ((properties :initarg :properties
               :initform nil
               :reader schema-properties
               :documentation "Alist of (name . schema) pairs."))
  (:documentation "Schema for object values."))

(defclass enum-schema (schema)
  ((values :initarg :values
           :initform nil
           :reader schema-enum-values
           :documentation "List of allowed values."))
  (:documentation "Schema for enumerated values."))

(defclass any-schema (schema)
  ()
  (:documentation "Schema that accepts any value."))

;;; ============================================================================
;;; Schema Type Protocol
;;; ============================================================================

(defmethod schema-type ((schema string-schema)) :string)
(defmethod schema-type ((schema number-schema)) :number)
(defmethod schema-type ((schema boolean-schema)) :boolean)
(defmethod schema-type ((schema array-schema)) :array)
(defmethod schema-type ((schema object-schema)) :object)
(defmethod schema-type ((schema enum-schema)) :enum)
(defmethod schema-type ((schema any-schema)) :any)

(defmethod schema-properties ((schema schema)) nil)
(defmethod schema-items ((schema schema)) nil)

;;; ============================================================================
;;; Schema Construction
;;; ============================================================================

(defun make-string-schema (&key required-p description min-length max-length pattern)
  (make-instance 'string-schema
                 :required-p required-p
                 :description description
                 :min-length min-length
                 :max-length max-length
                 :pattern pattern))

(defun make-number-schema (&key required-p description minimum maximum integer-p)
  (make-instance 'number-schema
                 :required-p required-p
                 :description description
                 :minimum minimum
                 :maximum maximum
                 :integer-p integer-p))

(defun make-boolean-schema (&key required-p description)
  (make-instance 'boolean-schema
                 :required-p required-p
                 :description description))

(defun make-array-schema (&key required-p description items min-items max-items)
  (make-instance 'array-schema
                 :required-p required-p
                 :description description
                 :items items
                 :min-items min-items
                 :max-items max-items))

(defun make-object-schema (&key required-p description properties)
  (make-instance 'object-schema
                 :required-p required-p
                 :description description
                 :properties properties))

(defun make-enum-schema (&key required-p description values)
  (make-instance 'enum-schema
                 :required-p required-p
                 :description description
                 :values values))

;;; ============================================================================
;;; Schema Parsing Helpers
;;; ============================================================================

(defun ends-with-p (string suffix)
  "Return T if STRING ends with SUFFIX."
  (let ((string-len (length string))
        (suffix-len (length suffix)))
    (and (>= string-len suffix-len)
         (string= suffix string :start2 (- string-len suffix-len)))))

;;; ============================================================================
;;; Schema Parsing
;;; ============================================================================

(defmethod parse (source (type (eql :schema)) &key)
  "Parse a schema definition from SOURCE."
  (etypecase source
    (hash-table (parse-schema-from-hash source))
    (string (parse-schema-from-string source))
    (null nil)))

(defun parse-schema-from-hash (hash)
  "Parse schema from a hash-table (YAML parsed object schema)."
  (let ((properties nil))
    (maphash (lambda (name type-spec)
               ;; Strip trailing ? from name (it's used for optional marker)
               (let ((clean-name (if (ends-with-p name "?")
                                     (subseq name 0 (1- (length name)))
                                     name)))
                 (push (cons clean-name (parse-field-schema name type-spec))
                       properties)))
             hash)
    (make-object-schema :properties (nreverse properties))))

(defun parse-schema-from-string (spec)
  "Parse a simple type spec string like 'string' or 'number'."
  (let ((type-str (string-downcase (string-trim '(#\Space #\Tab) spec))))
    (cond
      ((string= type-str "string") (make-string-schema :required-p t))
      ((string= type-str "number") (make-number-schema :required-p t))
      ((string= type-str "integer") (make-number-schema :required-p t :integer-p t))
      ((string= type-str "boolean") (make-boolean-schema :required-p t))
      ((string= type-str "array") (make-array-schema :required-p t))
      ((string= type-str "object") (make-object-schema :required-p t))
      ((string= type-str "any") (make-instance 'any-schema :required-p t))
      (t (make-instance 'any-schema :required-p t)))))

(defun parse-field-schema (name type-spec)
  "Parse a field schema from NAME and TYPE-SPEC.
TYPE-SPEC can be a string like 'string' or 'string?, description here'."
  (let* ((spec-str (if (stringp type-spec) type-spec (princ-to-string type-spec)))
         (optional-p (ends-with-p name "?"))
         (clean-name (if optional-p
                         (subseq name 0 (1- (length name)))
                         name)))
    (declare (ignore clean-name))
    ;; Parse "type?, description" or "type, description" format
    (multiple-value-bind (type-part description)
        (parse-type-spec spec-str)
      (let* ((type-optional-p (or optional-p (ends-with-p type-part "?")))
             (base-type (string-trim '(#\? #\Space) type-part)))
        (let ((schema (parse-schema-from-string base-type)))
          ;; Update required-p based on optional marker
          (reinitialize-instance schema
                                 :required-p (not type-optional-p)
                                 :description description))))))

(defun parse-type-spec (spec)
  "Parse 'type, description' into (values type description)."
  (let ((comma-pos (position #\, spec)))
    (if comma-pos
        (values (subseq spec 0 comma-pos)
                (string-trim '(#\Space #\Tab) (subseq spec (1+ comma-pos))))
        (values spec nil))))


;;; ============================================================================
;;; Validation Protocol Implementation
;;; ============================================================================

(defmethod validate ((schema schema) value &key (path nil))
  "Default validation - accepts any value."
  (declare (ignore path))
  value)

(defmethod validate ((schema string-schema) value &key (path nil))
  "Validate VALUE as a string."
  (restart-case
      (progn
        (unless (stringp value)
          (error 'type-validation-error
                 :expected-type :string
                 :value value
                 :path path))
        (when-let (min (schema-min-length schema))
          (when (< (length value) min)
            (error 'validation-error
                   :message (format nil "String length ~A is less than minimum ~A"
                                    (length value) min)
                   :value value
                   :path path)))
        (when-let (max (schema-max-length schema))
          (when (> (length value) max)
            (error 'validation-error
                   :message (format nil "String length ~A exceeds maximum ~A"
                                    (length value) max)
                   :value value
                   :path path)))
        (when-let (pattern (schema-pattern schema))
          (unless (cl-ppcre:scan pattern value)
            (error 'validation-error
                   :message (format nil "String does not match pattern ~A" pattern)
                   :value value
                   :path path)))
        value)
    (skip-validation ()
      :report "Skip validation and use value as-is"
      value)
    (provide-value (new-value)
      :report "Provide a different value"
      :interactive (lambda () (list (read-line)))
      new-value)))

(defmethod validate ((schema number-schema) value &key (path nil))
  "Validate VALUE as a number."
  (restart-case
      (progn
        (unless (numberp value)
          (error 'type-validation-error
                 :expected-type :number
                 :value value
                 :path path))
        (when (and (schema-integer-p schema) (not (integerp value)))
          (error 'type-validation-error
                 :expected-type :integer
                 :value value
                 :path path))
        (when-let (min (schema-minimum schema))
          (when (< value min)
            (error 'validation-error
                   :message (format nil "Number ~A is less than minimum ~A" value min)
                   :value value
                   :path path)))
        (when-let (max (schema-maximum schema))
          (when (> value max)
            (error 'validation-error
                   :message (format nil "Number ~A exceeds maximum ~A" value max)
                   :value value
                   :path path)))
        value)
    (skip-validation ()
      :report "Skip validation and use value as-is"
      value)))

(defmethod validate ((schema boolean-schema) value &key (path nil))
  "Validate VALUE as a boolean."
  (restart-case
      (progn
        (unless (or (eq value t) (eq value nil)
                    (eq value :true) (eq value :false))
          (error 'type-validation-error
                 :expected-type :boolean
                 :value value
                 :path path))
        value)
    (skip-validation ()
      :report "Skip validation and use value as-is"
      value)))

(defmethod validate ((schema array-schema) value &key (path nil))
  "Validate VALUE as an array/list."
  (restart-case
      (progn
        (unless (listp value)
          (error 'type-validation-error
                 :expected-type :array
                 :value value
                 :path path))
        (when-let (min (schema-min-items schema))
          (when (< (length value) min)
            (error 'validation-error
                   :message (format nil "Array length ~A is less than minimum ~A"
                                    (length value) min)
                   :value value
                   :path path)))
        (when-let (max (schema-max-items schema))
          (when (> (length value) max)
            (error 'validation-error
                   :message (format nil "Array length ~A exceeds maximum ~A"
                                    (length value) max)
                   :value value
                   :path path)))
        (when-let (items-schema (schema-items schema))
          (loop for item in value
                for i from 0
                do (validate items-schema item :path (append path (list i)))))
        value)
    (skip-validation ()
      :report "Skip validation and use value as-is"
      value)))

(defmethod validate ((schema object-schema) value &key (path nil))
  "Validate VALUE as an object."
  (restart-case
      (let ((value-hash (normalize-to-hash value)))
        (dolist (prop-pair (schema-properties schema))
          (let* ((prop-name (car prop-pair))
                 (prop-schema (cdr prop-pair))
                 (prop-value (gethash prop-name value-hash)))
            (if prop-value
                (validate prop-schema prop-value
                          :path (append path (list prop-name)))
                (when (schema-required-p prop-schema)
                  (error 'missing-required-error
                         :field prop-name
                         :path path)))))
        value)
    (skip-validation ()
      :report "Skip validation and use value as-is"
      value)))

(defmethod validate ((schema enum-schema) value &key (path nil))
  "Validate VALUE against enum values."
  (restart-case
      (progn
        (unless (member value (schema-enum-values schema) :test #'equal)
          (error 'validation-error
                 :message (format nil "Value must be one of: ~{~A~^, ~}"
                                  (schema-enum-values schema))
                 :value value
                 :path path))
        value)
    (skip-validation ()
      :report "Skip validation and use value as-is"
      value)))

(defun normalize-to-hash (value)
  "Normalize VALUE to a hash-table for object validation."
  (etypecase value
    (hash-table value)
    (list (if (null value)
              (make-hash-table :test 'equal)
              (if (keywordp (first value))
                  ;; Plist - convert keyword keys to strings
                  (let ((h (make-hash-table :test 'equal)))
                    (loop for (k v) on value by #'cddr
                          do (setf (gethash (string-downcase (symbol-name k)) h) v))
                    h)
                  ;; Alist
                  (alist-hash-table value :test 'equal))))))

;;; ============================================================================
;;; High-Level Validation API
;;; ============================================================================

(defmethod validate-input ((prompt dotprompt) input)
  "Validate INPUT against PROMPT's input schema."
  (if-let (schema (prompt-input-schema prompt))
    (validate schema input)
    input))
