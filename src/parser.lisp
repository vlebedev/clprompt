;;;; parser.lisp - .prompt file parsing for clprompt

(in-package #:clprompt)

;;; ============================================================================
;;; Constants
;;; ============================================================================

(alexandria:define-constant +frontmatter-delimiter+ "---"
  :test #'string=
  :documentation "YAML frontmatter delimiter.")

;;; ============================================================================
;;; Frontmatter Parsing
;;; ============================================================================

(defun split-frontmatter (text)
  "Split TEXT into frontmatter and template body.

Returns two values: the frontmatter string (without delimiters) and the
template body string. If no frontmatter is present, returns NIL and TEXT."
  (let ((delimiter-length (length +frontmatter-delimiter+)))
    (if (and (>= (length text) delimiter-length)
             (string= +frontmatter-delimiter+ text
                      :end2 delimiter-length))
        ;; Starts with ---, look for closing ---
        (let* ((start (+ delimiter-length
                         (if (and (> (length text) delimiter-length)
                                  (char= (char text delimiter-length) #\Newline))
                             1 0)))
               (end-pos (search (format nil "~%~A~%" +frontmatter-delimiter+)
                                text :start2 start)))
          (if end-pos
              (values (subseq text start end-pos)
                      (let ((body-start (+ end-pos 1 delimiter-length 1)))
                        (if (< body-start (length text))
                            (string-left-trim '(#\Newline) (subseq text body-start))
                            "")))
              ;; No closing delimiter found
              (error 'frontmatter-error
                     :message "Unclosed frontmatter block"
                     :source text)))
        ;; No frontmatter
        (values nil text))))

(defun parse-frontmatter (yaml-string)
  "Parse YAML-STRING into a property list.

Returns a plist with keys :MODEL, :CONFIG, :INPUT, :OUTPUT, :METADATA."
  (restart-case
      (handler-case
          (let ((yaml-data (yaml:parse yaml-string)))
            (extract-frontmatter-data yaml-data))
        (error (e)
          (error 'frontmatter-error
                 :message (format nil "YAML parse error: ~A" e)
                 :source yaml-string)))
    (use-default ()
      :report "Use empty frontmatter"
      (list :model nil :config nil :input nil :output nil :metadata nil))))

(defun extract-frontmatter-data (yaml-data)
  "Extract structured data from parsed YAML hash-table."
  (let ((model (gethash "model" yaml-data))
        (input (gethash "input" yaml-data))
        (output (gethash "output" yaml-data))
        (config (extract-config yaml-data))
        (metadata (extract-metadata yaml-data)))
    (list :model model
          :config config
          :input input
          :output output
          :metadata metadata)))

(defun extract-config (yaml-data)
  "Extract configuration parameters from YAML-DATA."
  (let ((config nil))
    (when-let (temp (gethash "temperature" yaml-data))
      (push temp config)
      (push :temperature config))
    (when-let (max-tok (gethash "maxOutputTokens" yaml-data))
      (push max-tok config)
      (push :max-tokens config))
    (when-let (top-p (gethash "topP" yaml-data))
      (push top-p config)
      (push :top-p config))
    (when-let (top-k (gethash "topK" yaml-data))
      (push top-k config)
      (push :top-k config))
    (when-let (stop (gethash "stopSequences" yaml-data))
      (push (ensure-list stop) config)
      (push :stop-sequences config))
    config))

(defun extract-metadata (yaml-data)
  "Extract non-standard metadata from YAML-DATA."
  (let ((known-keys '("model" "input" "output" "temperature"
                      "maxOutputTokens" "topP" "topK" "stopSequences"))
        (metadata nil))
    (maphash (lambda (key value)
               (unless (member key known-keys :test #'string=)
                 (push value metadata)
                 (push (intern (string-upcase key) :keyword) metadata)))
             yaml-data)
    metadata))

;;; ============================================================================
;;; Parse Protocol Implementation
;;; ============================================================================

(defmethod parse (source (type (eql :prompt)) &key)
  "Parse a complete .prompt file from SOURCE string."
  (multiple-value-bind (frontmatter-str template-str)
      (split-frontmatter source)
    (let* ((frontmatter (if frontmatter-str
                            (parse-frontmatter frontmatter-str)
                            (list :model nil :config nil :input nil
                                  :output nil :metadata nil)))
           (template (parse template-str :template))
           (input-schema (when-let (input (getf frontmatter :input))
                           (parse-input-schema input)))
           (output-data (getf frontmatter :output))
           (output-schema (when output-data
                            (parse-output-schema output-data)))
           (output-format (when output-data
                            (parse-output-format output-data))))
      (make-dotprompt
       :model (getf frontmatter :model)
       :config (getf frontmatter :config)
       :input-schema input-schema
       :output-schema output-schema
       :output-format output-format
       :template template
       :metadata (getf frontmatter :metadata)))))

(defmethod parse (source (type (eql :frontmatter)) &key)
  "Parse YAML frontmatter from SOURCE string."
  (parse-frontmatter source))

;;; ============================================================================
;;; Schema Parsing Helpers (Stubs - implemented in schema.lisp)
;;; ============================================================================

(defun parse-input-schema (input-data)
  "Parse input schema from frontmatter INPUT-DATA.
Returns a schema object or NIL."
  ;; Will be implemented in schema.lisp
  (when-let (schema-data (if (hash-table-p input-data)
                             (gethash "schema" input-data)
                             input-data))
    (parse schema-data :schema)))

(defun parse-output-schema (output-data)
  "Parse output schema from frontmatter OUTPUT-DATA.
Returns a schema object or NIL."
  ;; Will be implemented in schema.lisp
  (when (hash-table-p output-data)
    (when-let (schema-data (gethash "schema" output-data))
      (parse schema-data :schema))))

(defun parse-output-format (output-data)
  "Extract output format from OUTPUT-DATA."
  (when (hash-table-p output-data)
    (let ((format (gethash "format" output-data)))
      (cond
        ((string-equal format "json") :json)
        ((string-equal format "text") :text)
        (t :text)))))

;;; ============================================================================
;;; File Loading
;;; ============================================================================

(defmethod load-prompt ((source pathname) &key)
  "Load a dotprompt from a file PATHNAME."
  (let ((content (uiop:read-file-string source)))
    (parse content :prompt)))

(defun looks-like-path-p (string)
  "Return T if STRING looks like a file path rather than prompt content."
  (and (< (length string) 256)
       (not (find #\Newline string))
       (not (str:starts-with-p "---" string))))

(defmethod load-prompt ((source string) &key)
  "Load a dotprompt from a SOURCE string.
If SOURCE looks like a pathname, read from file; otherwise parse as content."
  (if (and (looks-like-path-p source)
           (probe-file source))
      (load-prompt (pathname source))
      (parse source :prompt)))

(defmethod load-prompt ((source stream) &key)
  "Load a dotprompt from a STREAM."
  (let ((content (alexandria:read-stream-content-into-string source)))
    (parse content :prompt)))
