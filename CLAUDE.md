# CLAUDE.md - Project Guidelines for clprompt

**Note**: This project uses [bd (beads)](https://github.com/steveyegge/beads)
for issue tracking. Use `bd` commands instead of markdown TODOs.
See AGENTS.md for workflow details.

## Project Overview

clprompt is a Common Lisp implementation of Google's [dotprompt](https://github.com/google/dotprompt) - an executable prompt template system for generative AI. The project follows rigorous CLOS-based object-oriented design principles.

## Build & Run Commands

```bash
# Load the system (from REPL)
(ql:quickload :clprompt)

# Run tests
(asdf:test-system :clprompt)

# Or from command line with SBCL
sbcl --eval '(ql:quickload :clprompt)' --eval '(asdf:test-system :clprompt)' --quit
```

## Project Structure

```
clprompt/
├── clprompt.asd          # ASDF system definition
├── src/
│   ├── package.lisp      # Package definition
│   ├── conditions.lisp   # Condition hierarchy
│   ├── protocol.lisp     # Generic function definitions (protocols)
│   ├── model.lisp        # Core domain classes
│   ├── parser.lisp       # .prompt file parsing
│   ├── template.lisp     # Handlebars template engine
│   ├── schema.lisp       # Schema validation
│   └── providers/        # LLM provider implementations
│       ├── protocol.lisp
│       ├── gemini.lisp
│       └── openai.lisp
├── tests/
│   └── *.lisp
└── examples/
    └── *.prompt
```

## CLOS Design Principles

### Protocol-Oriented Design

Define protocols (sets of generic functions) BEFORE implementing classes. Protocols represent abstract contracts:

```lisp
;;; GOOD - Protocol defined separately
(defgeneric render (template context)
  (:documentation "Render TEMPLATE with CONTEXT, returning a string."))

(defgeneric validate (schema value)
  (:documentation "Validate VALUE against SCHEMA. Signal VALIDATION-ERROR on failure."))

;;; Classes implement protocols
(defclass handlebars-template (template)
  ((source :initarg :source :reader template-source)
   (compiled :accessor template-compiled)))

(defmethod render ((template handlebars-template) context)
  ...)
```

### Class Design Guidelines

1. **Prefer composition over inheritance** - Use mixins and delegation
2. **Keep inheritance hierarchies shallow** - Rarely exceed 3 levels
3. **Use abstract base classes** - Define protocol classes with no slots
4. **Immutable by default** - Use `:reader` over `:accessor` when mutation isn't needed
5. **Initialize properly** - Use `:initform` and `initialize-instance :after` methods

```lisp
;;; Protocol class (abstract)
(defclass provider ()
  ()
  (:documentation "Abstract base for LLM providers."))

;;; Mixin for authentication
(defclass api-key-mixin ()
  ((api-key :initarg :api-key
            :reader provider-api-key
            :initform (error "API-KEY is required"))))

;;; Concrete implementation using composition
(defclass gemini-provider (provider api-key-mixin)
  ((model :initarg :model
          :reader provider-model
          :initform "gemini-1.5-pro")))
```

### Method Combination

Use standard method combination effectively:

- `:before` - Validation, logging, setup
- `:after` - Cleanup, notifications, caching
- `:around` - Transformation, conditional execution, memoization
- Primary methods - Core logic

```lisp
(defmethod validate :before ((schema schema) value)
  "Log validation attempts."
  (log:debug "Validating ~A against ~A" value schema))

(defmethod render :around ((template cached-template) context)
  "Return cached result if available."
  (or (gethash context (template-cache template))
      (setf (gethash context (template-cache template))
            (call-next-method))))
```

### Condition System

Use the condition system for error handling, not return values:

```lisp
;;; Define condition hierarchy
(define-condition clprompt-error (error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (c s) (format s "~A" (error-message c)))))

(define-condition parse-error (clprompt-error)
  ((line :initarg :line :reader error-line)
   (column :initarg :column :reader error-column)))

(define-condition validation-error (clprompt-error)
  ((schema :initarg :schema :reader error-schema)
   (value :initarg :value :reader error-value)))

;;; Use restarts for recovery
(defun parse-frontmatter (text)
  (restart-case
      (or (parse-yaml text)
          (error 'parse-error :message "Invalid YAML" :line 1 :column 0))
    (use-default ()
      :report "Use default frontmatter"
      (make-default-frontmatter))
    (provide-value (value)
      :report "Provide frontmatter manually"
      :interactive (lambda () (list (read)))
      value)))
```

### Slot Access Conventions

- Use `defclass` slot readers/accessors, not `slot-value` directly
- Name readers after the concept: `template-source`, not `get-template-source`
- Boolean predicates end in `-p`: `optional-p`, `required-p`
- Use `with-accessors` or `with-slots` sparingly, prefer explicit reader calls

### Multiple Dispatch

Leverage multiple dispatch for clean, extensible code:

```lisp
;;; Different rendering strategies based on both template and output types
(defgeneric render-to (template context output-type)
  (:documentation "Render TEMPLATE to specific OUTPUT-TYPE."))

(defmethod render-to ((tmpl handlebars-template) ctx (type (eql :string)))
  ...)

(defmethod render-to ((tmpl handlebars-template) ctx (type (eql :stream)))
  ...)
```

### MOP Usage (When Appropriate)

Use the MOP judiciously for metaprogramming needs:

```lisp
;;; Custom metaclass for schema-validated slots
(defclass validated-class (standard-class)
  ())

(defmethod validate-superclass ((class validated-class) (super standard-class))
  t)

(defmethod (setf slot-value-using-class) :before
    (new-value (class validated-class) object slot)
  (let ((schema (slot-schema slot)))
    (when schema
      (validate schema new-value))))
```

## Code Style

### Naming Conventions

- Classes: `kebab-case` (e.g., `handlebars-template`, `gemini-provider`)
- Generic functions: verbs in `kebab-case` (e.g., `render`, `validate`, `parse-template`)
- Slot readers: `class-slot` pattern (e.g., `template-source`, `provider-api-key`)
- Predicates: end with `-p` (e.g., `valid-p`, `optional-p`)
- Special variables: `*earmuffs*` (e.g., `*default-provider*`)
- Constants: `+plus-signs+` (e.g., `+max-retries+`)

### Documentation

- Every exported symbol must have a docstring
- Use `:documentation` in `defclass` and `defgeneric`
- Document non-obvious slot purposes

### Formatting

- 80 character line limit (soft), 100 (hard)
- 2-space indentation
- Align `defclass` slot options vertically
- One blank line between top-level forms
- Two blank lines between sections

## Testing Guidelines

Use FiveAM for testing:

```lisp
(def-suite clprompt-tests
  :description "Main test suite for clprompt")

(def-suite parser-tests
  :in clprompt-tests)

(in-suite parser-tests)

(test parse-simple-frontmatter
  "Parsing YAML frontmatter extracts model configuration."
  (let ((result (parse-frontmatter "model: gemini-1.5-pro")))
    (is (equal "gemini-1.5-pro" (frontmatter-model result)))))

(test parse-invalid-yaml-signals-error
  "Invalid YAML signals parse-error condition."
  (signals parse-error
    (parse-frontmatter "invalid: yaml: content:")))
```

## Dependencies

- `cl-yaml` - YAML parsing
- `yason` - JSON encoding/decoding
- `dexador` - HTTP client
- `cl-ppcre` - Regular expressions
- `alexandria` - Common utilities
- `str` - String manipulation
- `fiveam` - Testing framework

## Git Workflow

- Commit messages: imperative mood, 50 char subject line
- One logical change per commit
- Run tests before committing
