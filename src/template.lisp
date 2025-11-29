;;;; template.lisp - Handlebars template engine for clprompt

(in-package #:clprompt)

;;; ============================================================================
;;; Template Helpers Registry
;;; ============================================================================

(defvar *template-helpers* (make-hash-table :test 'equal)
  "Registry of template helper functions.
Keys are helper names (strings), values are functions that take a value and return a string.")

(defun register-helper (name function)
  "Register a template helper FUNCTION under NAME.
FUNCTION should take a single value and return a string."
  (setf (gethash name *template-helpers*) function)
  name)

(defun get-helper (name)
  "Get the helper function registered under NAME, or NIL if not found."
  (gethash name *template-helpers*))

(defun helper-registered-p (name)
  "Return T if a helper is registered under NAME."
  (nth-value 1 (gethash name *template-helpers*)))

;;; Built-in helpers

(register-helper "json"
  (lambda (value)
    "Serialize VALUE to JSON string."
    (with-output-to-string (s)
      (yason:encode value s))))

;;; ============================================================================
;;; Template Classes
;;; ============================================================================

(defclass template ()
  ()
  (:documentation "Abstract base class for templates."))

(defclass handlebars-template (template)
  ((source :initarg :source
           :initform (error "SOURCE is required")
           :reader template-source
           :documentation "Original template source string.")
   (compiled :initform nil
             :accessor %template-compiled
             :documentation "Compiled template representation.")
   (compiled-p :initform nil
               :accessor %template-compiled-p
               :documentation "Whether the template has been compiled."))
  (:documentation "A Handlebars-syntax template."))

(defmethod print-object ((template handlebars-template) stream)
  (print-unreadable-object (template stream :type t)
    (let ((source (template-source template)))
      (format stream "~S~@[...~]"
              (subseq source 0 (min 30 (length source)))
              (> (length source) 30)))))

(defmethod template-compiled-p ((template handlebars-template))
  (%template-compiled-p template))

;;; ============================================================================
;;; Template Construction
;;; ============================================================================

(defun make-handlebars-template (source)
  "Create a new Handlebars template from SOURCE string."
  (make-instance 'handlebars-template :source source))

;;; ============================================================================
;;; Parse Protocol Implementation
;;; ============================================================================

(defmethod parse (source (type (eql :template)) &key)
  "Parse a Handlebars template from SOURCE string."
  (make-handlebars-template source))

;;; ============================================================================
;;; Template Compilation
;;; ============================================================================

;;; The compiled representation is a list of segments, where each segment is:
;;; - A string literal
;;; - A list (:VAR name) for variable substitution
;;; - A list (:VAR-ESCAPED name) for HTML-escaped variable
;;; - A list (:BLOCK type name body else-body) for block expressions

(defparameter *handlebars-pattern*
  (cl-ppcre:create-scanner
   "\\{\\{(!--|!|#|/|\\^|>|&)?\\s*([^}]+?)\\s*\\}\\}"
   :single-line-mode t)
  "Scanner for Handlebars expressions.")

(defmethod compile-template ((template handlebars-template))
  "Compile TEMPLATE into an efficient representation."
  (unless (template-compiled-p template)
    (setf (%template-compiled template)
          (compile-handlebars-source (template-source template)))
    (setf (%template-compiled-p template) t))
  template)

(defun compile-handlebars-source (source)
  "Compile Handlebars SOURCE into a list of segments."
  (let ((segments nil)
        (pos 0)
        (length (length source)))
    (cl-ppcre:do-matches (start end *handlebars-pattern* source)
      ;; Add literal text before this match
      (when (> start pos)
        (push (subseq source pos start) segments))
      ;; Parse the handlebars expression
      (multiple-value-bind (match-start match-end reg-starts reg-ends)
          (cl-ppcre:scan *handlebars-pattern* source :start pos)
        (declare (ignore match-start match-end))
        (let* ((prefix (when (aref reg-starts 0)
                         (subseq source (aref reg-starts 0) (aref reg-ends 0))))
               (content (subseq source (aref reg-starts 1) (aref reg-ends 1))))
          (push (parse-handlebars-expression prefix content) segments)))
      (setf pos end))
    ;; Add remaining literal text
    (when (< pos length)
      (push (subseq source pos) segments))
    (nreverse segments)))

(defun parse-handlebars-expression (prefix content)
  "Parse a single Handlebars expression with PREFIX and CONTENT."
  (cond
    ;; Comment: {{!-- comment --}} or {{! comment }}
    ((or (string= prefix "!--") (string= prefix "!"))
     (list :comment content))
    ;; Block start: {{#if condition}}
    ((string= prefix "#")
     (let* ((parts (cl-ppcre:split "\\s+" content :limit 2))
            (block-type (first parts))
            (block-arg (second parts)))
       (list :block-start (intern (string-upcase block-type) :keyword) block-arg)))
    ;; Block end: {{/if}}
    ((string= prefix "/")
     (list :block-end (intern (string-upcase content) :keyword)))
    ;; Inverse block: {{^}}
    ((string= prefix "^")
     (if (zerop (length content))
         (list :else)
         (list :block-start :unless content)))
    ;; Partial: {{> partial}}
    ((string= prefix ">")
     (list :partial content))
    ;; Unescaped variable: {{& var}} or {{{ var }}}
    ((string= prefix "&")
     (list :var content))
    ;; Check for helper call: {{helper arg}} where helper is registered
    ((and (null prefix)
          (let ((parts (cl-ppcre:split "\\s+" content :limit 2)))
            (and (= (length parts) 2)
                 (helper-registered-p (first parts)))))
     (let ((parts (cl-ppcre:split "\\s+" content :limit 2)))
       (list :helper (first parts) (second parts))))
    ;; Regular variable (escaped by default in Handlebars, but we don't escape for LLM prompts)
    (t
     (list :var content))))

;;; ============================================================================
;;; Template Rendering
;;; ============================================================================

(defmethod render ((template handlebars-template) context)
  "Render TEMPLATE with CONTEXT."
  (compile-template template)
  (let ((ctx (normalize-context context)))
    (render-segments (%template-compiled template) ctx)))

(defun normalize-context (context)
  "Normalize CONTEXT to a hash-table with string keys."
  (etypecase context
    (hash-table context)
    (list (if (and context (keywordp (first context)))
              ;; Convert plist with keyword keys to hash-table with string keys
              (let ((ht (make-hash-table :test 'equal)))
                (loop for (k v) on context by #'cddr
                      do (setf (gethash (string-downcase (symbol-name k)) ht) v))
                ht)
              ;; Alist - keep string keys
              (alist-hash-table context :test 'equal)))
    (null (make-hash-table :test 'equal))))

(defun render-segments (segments context)
  "Render SEGMENTS with CONTEXT, returning a string."
  (with-output-to-string (out)
    (render-segments-to-stream segments context out)))

(defun render-segments-to-stream (segments context stream)
  "Render SEGMENTS to STREAM with CONTEXT."
  (let ((i 0)
        (len (length segments)))
    (loop while (< i len) do
      (let ((segment (nth i segments)))
        (cond
          ;; String literal
          ((stringp segment)
           (write-string segment stream))
          ;; Variable
          ((and (listp segment) (eq (first segment) :var))
           (let ((value (resolve-variable (second segment) context)))
             (when value
               (princ value stream))))
          ;; Helper call
          ((and (listp segment) (eq (first segment) :helper))
           (let* ((helper-name (second segment))
                  (arg-name (third segment))
                  (helper-fn (get-helper helper-name))
                  (value (resolve-variable arg-name context)))
             (when (and helper-fn value)
               (let ((result (funcall helper-fn value)))
                 (when result
                   (write-string result stream))))))
          ;; Comment - skip
          ((and (listp segment) (eq (first segment) :comment))
           nil)
          ;; Block start - need to find matching end and render block
          ((and (listp segment) (eq (first segment) :block-start))
           (multiple-value-bind (block-segments end-index)
               (collect-block-segments segments (1+ i) (second segment))
             (render-block (second segment) (third segment)
                           block-segments context stream)
             (setf i end-index)))
          ;; Partial - not yet implemented
          ((and (listp segment) (eq (first segment) :partial))
           (warn "Partials not yet implemented: ~A" (second segment)))
          (t
           (warn "Unknown segment type: ~A" segment))))
      (incf i))))

(defun resolve-variable (name context)
  "Resolve variable NAME in CONTEXT, supporting dotted paths."
  (let ((parts (cl-ppcre:split "\\." name)))
    (reduce (lambda (ctx part)
              (when ctx
                (etypecase ctx
                  (hash-table (gethash part ctx))
                  (list (if (keywordp (first ctx))
                            (getf ctx (intern (string-upcase part) :keyword))
                            (cdr (assoc part ctx :test #'string=)))))))
            parts
            :initial-value context)))

(defun collect-block-segments (segments start block-type)
  "Collect segments for a block, handling nesting.
Returns (values block-segments end-index)."
  (let ((depth 1)
        (block-segs nil)
        (i start))
    (loop while (and (< i (length segments)) (> depth 0)) do
      (let ((seg (nth i segments)))
        (cond
          ((and (listp seg) (eq (first seg) :block-start)
                (eq (second seg) block-type))
           (incf depth)
           (push seg block-segs))
          ((and (listp seg) (eq (first seg) :block-end)
                (eq (second seg) block-type))
           (decf depth)
           (unless (zerop depth)
             (push seg block-segs)))
          (t
           (push seg block-segs))))
      (incf i))
    (values (nreverse block-segs) i)))

(defun render-block (block-type arg block-segments context stream)
  "Render a block expression."
  (case block-type
    (:if
     (let ((value (resolve-variable arg context)))
       (when (and value (not (eq value :false)) (not (equal value "")))
         (render-segments-to-stream block-segments context stream))))
    (:unless
     (let ((value (resolve-variable arg context)))
       (unless (and value (not (eq value :false)) (not (equal value "")))
         (render-segments-to-stream block-segments context stream))))
    (:each
     (let ((items (resolve-variable arg context)))
       (when (listp items)
         (dolist (item items)
           (let ((item-context (etypecase item
                                 (hash-table item)
                                 (list (if (keywordp (first item))
                                           (plist-hash-table item :test 'equal)
                                           (alist-hash-table item :test 'equal)))
                                 (t (let ((h (make-hash-table :test 'equal)))
                                      (setf (gethash "this" h) item)
                                      h)))))
             ;; Merge parent context
             (maphash (lambda (k v)
                        (unless (gethash k item-context)
                          (setf (gethash k item-context) v)))
                      context)
             (render-segments-to-stream block-segments item-context stream))))))
    (:with
     (let ((value (resolve-variable arg context)))
       (when value
         (let ((new-context (etypecase value
                              (hash-table value)
                              (list (if (keywordp (first value))
                                        (plist-hash-table value :test 'equal)
                                        (alist-hash-table value :test 'equal))))))
           (render-segments-to-stream block-segments new-context stream)))))
    (t
     (warn "Unknown block type: ~A" block-type))))

;;; ============================================================================
;;; High-Level API Implementation
;;; ============================================================================

(defmethod render-prompt ((prompt dotprompt) input &key validate)
  "Render PROMPT's template with INPUT."
  (when validate
    (validate-input prompt input))
  (render (prompt-template prompt) input))
