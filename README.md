# clprompt

A Common Lisp implementation of Google's [dotprompt](https://github.com/google/dotprompt) format - an executable prompt template system for generative AI.

## Features

- Parse `.prompt` files with YAML frontmatter and Handlebars templates
- Handlebars template engine with variable substitution, conditionals, and iteration
- Schema validation for input/output
- LLM provider integrations (Gemini, OpenAI)
- CLOS-based extensible architecture

## Installation

Clone the repository and ensure it's in your ASDF load path:

```bash
git clone https://github.com/vlebedev/clprompt.git
```

### Dependencies

- `alexandria` - Common utilities
- `cl-ppcre` - Regular expressions
- `cl-yaml` - YAML parsing
- `yason` - JSON encoding/decoding
- `dexador` - HTTP client
- `str` - String manipulation
- `fiveam` - Testing (for development)

All dependencies are available via Quicklisp.

## Quick Start

### Using with SLY in Emacs

1. Start SLY: `M-x sly`

2. Register the project and load it in the REPL:

```lisp
(push #P"/home/wal/projects/clprompt/" asdf:*central-registry*)
(ql:quickload :clprompt)
```

3. Switch to the clprompt package for convenience:

```lisp
(in-package :clprompt)
```

4. Now you can interactively test:

```lisp
;; Load and render a prompt
(defparameter *p* (load-prompt "/home/wal/projects/clprompt/examples/summarize.prompt"))
(render-prompt *p* '(:content "Some long text here..." :style "brief"))

;; Run the test suite
(ql:quickload :clprompt/tests)
(clprompt/tests:run-tests)
```

**Useful SLY keybindings:**
- `C-c C-c` - Compile defun at point
- `C-c C-k` - Compile and load current buffer
- `C-c C-z` - Switch to REPL
- `M-.` - Jump to definition
- `C-c C-d d` - Describe symbol
- `C-c I` - Inspect value

**Reloading after code changes:**

```lisp
;; Option 1: Reload the entire system (recommended)
(asdf:load-system :clprompt :force t)

;; Option 2: Recompile specific file - open it in Emacs and press C-c C-k

;; Option 3: Reload from REPL
(load (compile-file "/home/wal/projects/clprompt/src/some-file.lisp"))
```

### Loading from Command Line

```bash
sbcl --eval '(push #P"/path/to/clprompt/" asdf:*central-registry*)' \
     --eval '(ql:quickload :clprompt)'
```

### Basic Usage

```lisp
;; Load a prompt from file
(defparameter *prompt*
  (clprompt:load-prompt "/path/to/examples/extract-info.prompt"))

;; Or parse inline
(defparameter *prompt*
  (clprompt:load-prompt "---
model: gemini-1.5-pro
temperature: 0.7
---
Hello {{name}}, tell me about {{topic}}."))

;; Render the template (without calling LLM)
(clprompt:render-prompt *prompt* '(:name "Claude" :topic "Common Lisp"))
;; => "Hello Claude, tell me about Common Lisp."

;; Inspect the prompt object
(clprompt:prompt-model *prompt*)   ; => "gemini-1.5-pro"
(clprompt:prompt-config *prompt*)  ; => (:TEMPERATURE 0.7)
```

### Template Rendering

```lisp
;; Direct template usage
(let ((tmpl (clprompt::make-handlebars-template
             "Items: {{#each items}}{{this}}, {{/each}}")))
  (clprompt:render tmpl '(:items ("apple" "banana" "cherry"))))
;; => "Items: apple, banana, cherry, "

;; Conditionals
(let ((tmpl (clprompt::make-handlebars-template
             "{{#if verbose}}Detailed output{{/if}}")))
  (clprompt:render tmpl '(:verbose t)))
;; => "Detailed output"

;; Dotted path access
(let ((tmpl (clprompt::make-handlebars-template "Name: {{person.name}}")))
  (clprompt:render tmpl (alexandria:alist-hash-table
                         '(("person" . #.(alexandria:alist-hash-table
                                          '(("name" . "Alice"))
                                          :test 'equal)))
                         :test 'equal)))
;; => "Name: Alice"
```

### Schema Validation

```lisp
;; String validation
(let ((schema (clprompt::make-string-schema :min-length 3 :max-length 100)))
  (clprompt:validate schema "hello"))  ; => "hello"

;; Number validation with range
(let ((schema (clprompt::make-number-schema :minimum 0 :maximum 100)))
  (clprompt:validate schema 42))  ; => 42

;; Object validation
(let ((schema (clprompt::make-object-schema
               :properties (list (cons "name" (clprompt::make-string-schema))
                                 (cons "age" (clprompt::make-number-schema))))))
  (clprompt:validate schema '(:name "Bob" :age 30)))
```

### LLM Provider Usage

```lisp
;; Create a provider (API key from GOOGLE_API_KEY env var)
(defparameter *gemini* (make-instance 'clprompt:gemini-provider))

;; Or with explicit API key
(defparameter *gemini*
  (make-instance 'clprompt:gemini-provider
                 :api-key "your-api-key"
                 :model "gemini-1.5-flash"))

;; Send a request
(clprompt:send-request *gemini* "What is Common Lisp?")
;; => (:CONTENT "Common Lisp is..." :RAW #<hash-table> :USAGE ...)
```

### Listing Available Models

To find valid model names for a provider, use `list-models` or the pretty-print helpers:

```lisp
;; List all available Gemini models
(clprompt:list-models *gemini*)
;; => ((:NAME "gemini-1.5-pro" :DISPLAY-NAME "Gemini 1.5 Pro" :INPUT-TOKEN-LIMIT 1048576 ...)
;;     (:NAME "gemini-1.5-flash" :DISPLAY-NAME "Gemini 1.5 Flash" ...)
;;     ...)

;; Pretty-print available models
(clprompt:print-gemini-models *gemini*)
;; Available Gemini models:
;; Model Name                                                   Input      Output
;; ------------------------------------------------------------
;; gemini-1.5-pro                                               1048576    8192
;; gemini-1.5-flash                                             1048576    8192
;; ...

;; For OpenAI
(defparameter *openai* (make-instance 'clprompt:openai-provider))
(clprompt:print-openai-models *openai*)
```

Each model entry includes:
- `:name` - Model identifier to use in API requests
- `:display-name` - Human-readable name
- `:description` - Model description
- `:input-token-limit` / `:output-token-limit` - Token limits (Gemini)
- `:supported-methods` - Supported API methods (Gemini)

## Prompt File Format

Prompt files use YAML frontmatter followed by a Handlebars template:

```yaml
---
model: googleai/gemini-1.5-pro
temperature: 0.7
maxOutputTokens: 1000
input:
  schema:
    text: string, the text to analyze
    format?: string, optional output format
output:
  format: json
  schema:
    summary: string, brief summary
    keywords?: array, extracted keywords
---
Analyze the following text and extract key information.

{{#if format}}
Output in {{format}} format.
{{/if}}

Text: {{text}}
```

### Supported Frontmatter Fields

| Field | Description |
|-------|-------------|
| `model` | Model identifier (e.g., `googleai/gemini-1.5-pro`) |
| `temperature` | Sampling temperature (0.0-1.0) |
| `maxOutputTokens` | Maximum tokens in response |
| `topP` | Top-p sampling parameter |
| `topK` | Top-k sampling parameter |
| `input.schema` | Input validation schema |
| `output.format` | Output format (`json`, `text`) |
| `output.schema` | Output validation schema |

### Handlebars Template Features

- **Variables**: `{{name}}`, `{{person.address.city}}`
- **Conditionals**: `{{#if condition}}...{{/if}}`, `{{#unless condition}}...{{/unless}}`
- **Iteration**: `{{#each items}}{{this}}{{/each}}`
- **Comments**: `{{! comment }}` or `{{!-- comment --}}`

## Running Tests

```lisp
(ql:quickload :clprompt/tests)
(clprompt/tests:run-tests)
```

Or from the command line:

```bash
sbcl --eval '(push #P"/path/to/clprompt/" asdf:*central-registry*)' \
     --eval '(ql:quickload :clprompt/tests)' \
     --eval '(asdf:test-system :clprompt)' \
     --quit
```

## API Reference

### Core Functions

| Function | Description |
|----------|-------------|
| `load-prompt` | Load a prompt from file path, string, or stream |
| `render-prompt` | Render a prompt's template with input variables |
| `validate-input` | Validate input against prompt's schema |
| `execute-prompt` | Full pipeline: validate, render, call LLM, parse output |
| `list-models` | List available models from a provider |
| `send-request` | Send a prompt to an LLM provider |

### Classes

| Class | Description |
|-------|-------------|
| `dotprompt` | Parsed prompt with model, config, schemas, and template |
| `handlebars-template` | Compiled Handlebars template |
| `gemini-provider` | Google Gemini API provider |
| `openai-provider` | OpenAI API provider |
| `string-schema`, `number-schema`, etc. | Schema validation classes |

### Conditions

| Condition | Description |
|-----------|-------------|
| `clprompt-error` | Base condition for all errors |
| `prompt-parse-error` | Parse errors (YAML, template syntax) |
| `validation-error` | Schema validation failures |
| `provider-error` | LLM API errors |

## License

MIT

## Contributing

Contributions are welcome! Please ensure all tests pass before submitting a PR.
