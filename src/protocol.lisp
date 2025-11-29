;;;; protocol.lisp - Generic function protocols for clprompt

(in-package #:clprompt)

;;; ============================================================================
;;; Configuration Variables
;;; ============================================================================

(defvar *default-provider* nil
  "The default LLM provider instance to use when none is specified.")

(defvar *default-model* nil
  "The default model identifier to use when none is specified in a prompt.")

(defvar *template-cache-enabled-p* t
  "When non-NIL, compiled templates are cached for reuse.")

;;; ============================================================================
;;; Parsing Protocol
;;; ============================================================================

(defgeneric parse (source type &key &allow-other-keys)
  (:documentation
   "Parse SOURCE as TYPE, returning the parsed object.

TYPE is an EQL-specializer indicating what kind of parsing to perform:
  :prompt      - Parse a complete .prompt file
  :frontmatter - Parse YAML frontmatter
  :template    - Parse a Handlebars template
  :schema      - Parse a schema definition

Signals PARSE-ERROR on failure."))

;;; ============================================================================
;;; Template Protocol
;;; ============================================================================

(defgeneric compile-template (template)
  (:documentation
   "Compile TEMPLATE into an efficient internal representation.

Returns the compiled template object (may be the same object, mutated).
Compilation is idempotent - calling on an already-compiled template is a no-op."))

(defgeneric render (template context)
  (:documentation
   "Render TEMPLATE with CONTEXT, returning the rendered string.

TEMPLATE is a template object (e.g., HANDLEBARS-TEMPLATE).
CONTEXT is a hash-table or plist mapping variable names to values.

Signals TEMPLATE-ERROR if rendering fails.
Signals UNDEFINED-VARIABLE-ERROR if a required variable is missing."))

(defgeneric template-source (template)
  (:documentation
   "Return the original source string of TEMPLATE."))

(defgeneric template-compiled-p (template)
  (:documentation
   "Return T if TEMPLATE has been compiled, NIL otherwise."))

;;; ============================================================================
;;; Schema Protocol
;;; ============================================================================

(defgeneric validate (schema value &key path)
  (:documentation
   "Validate VALUE against SCHEMA.

PATH is a list of keys/indices representing the current location in a nested
structure (used for error reporting).

Returns VALUE if valid (possibly coerced to the expected type).
Signals VALIDATION-ERROR if VALUE does not conform to SCHEMA."))

(defgeneric schema-type (schema)
  (:documentation
   "Return a keyword indicating the type of SCHEMA.

Possible return values: :STRING, :NUMBER, :BOOLEAN, :ARRAY, :OBJECT, :ENUM, :ANY"))

(defgeneric schema-required-p (schema)
  (:documentation
   "Return T if SCHEMA represents a required field, NIL if optional."))

(defgeneric schema-description (schema)
  (:documentation
   "Return the human-readable description of SCHEMA, or NIL if none."))

(defgeneric schema-properties (schema)
  (:documentation
   "For object schemas, return an alist of (NAME . SCHEMA) pairs.
For non-object schemas, return NIL."))

(defgeneric schema-items (schema)
  (:documentation
   "For array schemas, return the schema for array items.
For non-array schemas, return NIL."))

(defgeneric coerce-value (schema value)
  (:documentation
   "Attempt to coerce VALUE to match SCHEMA's expected type.

Returns the coerced value if successful.
Signals VALIDATION-ERROR if coercion is not possible."))

;;; ============================================================================
;;; Provider Protocol
;;; ============================================================================

(defgeneric send-request (provider prompt &key &allow-other-keys)
  (:documentation
   "Send PROMPT to PROVIDER and return the response.

PROMPT is the rendered prompt string.
Additional keyword arguments may include:
  :model           - Override the provider's default model
  :temperature     - Sampling temperature
  :max-tokens      - Maximum tokens in response
  :output-schema   - Expected output schema for structured responses

Returns a plist with at least :CONTENT key containing the response text.
May include additional keys like :USAGE, :MODEL, :FINISH-REASON.

Signals PROVIDER-ERROR on failure.
Signals AUTHENTICATION-ERROR if API key is invalid.
Signals RATE-LIMIT-ERROR if rate limited."))

(defgeneric provider-name (provider)
  (:documentation
   "Return a string identifying the provider (e.g., \"gemini\", \"openai\")."))

(defgeneric provider-api-key (provider)
  (:documentation
   "Return the API key configured for PROVIDER."))

(defgeneric provider-model (provider)
  (:documentation
   "Return the default model identifier for PROVIDER."))

(defgeneric provider-endpoint (provider)
  (:documentation
   "Return the API endpoint URL for PROVIDER."))

(defgeneric provider-available-p (provider)
  (:documentation
   "Return T if PROVIDER is properly configured and reachable."))

;;; ============================================================================
;;; Dotprompt Protocol
;;; ============================================================================

(defgeneric prompt-model (prompt)
  (:documentation
   "Return the model specification from PROMPT's frontmatter."))

(defgeneric prompt-config (prompt)
  (:documentation
   "Return the configuration plist from PROMPT (temperature, max-tokens, etc)."))

(defgeneric prompt-input-schema (prompt)
  (:documentation
   "Return the input schema object for PROMPT, or NIL if none defined."))

(defgeneric prompt-output-schema (prompt)
  (:documentation
   "Return the output schema object for PROMPT, or NIL if none defined."))

(defgeneric prompt-output-format (prompt)
  (:documentation
   "Return the expected output format (:JSON, :TEXT, etc) for PROMPT."))

(defgeneric prompt-template (prompt)
  (:documentation
   "Return the template object for PROMPT."))

(defgeneric prompt-metadata (prompt)
  (:documentation
   "Return additional metadata plist from PROMPT's frontmatter."))

;;; ============================================================================
;;; Execution Protocol
;;; ============================================================================

(defgeneric execute (prompt input &key provider &allow-other-keys)
  (:documentation
   "Execute PROMPT with INPUT, returning the LLM response.

This is the high-level entry point that:
1. Validates INPUT against the prompt's input schema
2. Renders the template with INPUT
3. Sends the rendered prompt to PROVIDER
4. Parses/validates the response against the output schema

PROVIDER defaults to *DEFAULT-PROVIDER*.

Returns a plist containing:
  :CONTENT  - The response content (parsed if output schema defined)
  :RAW      - The raw response string
  :USAGE    - Token usage information (if available)

Signals VALIDATION-ERROR if input doesn't match schema.
Signals PROVIDER-ERROR if the LLM request fails."))

;;; ============================================================================
;;; High-Level API Protocol
;;; ============================================================================

(defgeneric load-prompt (source &key &allow-other-keys)
  (:documentation
   "Load a dotprompt from SOURCE.

SOURCE can be:
  - A pathname to a .prompt file
  - A string containing .prompt content
  - A stream

Returns a DOTPROMPT object.
Signals PARSE-ERROR if the prompt is malformed."))

(defgeneric render-prompt (prompt input &key &allow-other-keys)
  (:documentation
   "Render PROMPT's template with INPUT, returning the rendered string.

Does not send to an LLM - useful for inspection/debugging.
Optionally validates INPUT against the schema if :VALIDATE is non-NIL."))

(defgeneric validate-input (prompt input)
  (:documentation
   "Validate INPUT against PROMPT's input schema.

Returns INPUT if valid.
Signals VALIDATION-ERROR if invalid.
Returns INPUT unchanged if no schema is defined."))

