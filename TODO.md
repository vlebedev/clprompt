# Common Lisp implementation of google dotprompt format

## Description and reference implementations

See https://github.com/google/dotprompt

## TODO

### Phase 1: Project Foundation
- [x] Create ASDF system definition (`clprompt.asd`)
- [x] Set up package structure (`package.lisp`)
- [x] Add core dependencies (cl-yaml, cl-json/yason, drakma/dexador)
- [x] Create basic project structure (src/, tests/, examples/)

### Phase 2: File Parsing
- [x] Implement `.prompt` file loader
- [x] Parse YAML frontmatter (between `---` markers)
- [x] Extract template body (Handlebars portion)
- [x] Define `dotprompt` struct/class to hold parsed data
  - model specification
  - input schema
  - output schema/format
  - config (temperature, max-tokens, etc.)
  - template body

### Phase 3: Handlebars Template Engine
- [x] Implement basic variable substitution (`{{variable}}`)
- [x] Support dotted property access (`{{person.name}}`)
- [x] Handle `#if`/`#unless` conditionals
- [x] Handle `#each` iteration for lists
- [x] Support `#with` context switching
- [ ] Implement helper functions (e.g., `{{json variable}}`)
- [ ] Handle partial templates (optional)

### Phase 4: Schema Validation
- [x] Define schema DSL/parser for input schemas
- [x] Implement type validators (string, number, boolean, array, object)
- [x] Handle optional fields (marked with `?`)
- [x] Validate input against declared schema before rendering
- [x] Parse output schemas for structured response handling
- [x] Support enum types and custom constraints

### Phase 5: LLM Integration
- [x] Create generic model provider interface (CLOS protocol)
- [x] Implement Google Gemini provider
- [x] Implement OpenAI provider (optional)
- [ ] Implement Anthropic provider (optional)
- [ ] Handle streaming responses
- [ ] Parse structured JSON output according to schema

### Phase 6: High-Level API
- [x] `load-prompt` - Load and parse a .prompt file
- [x] `render-prompt` - Render template with input variables
- [ ] `execute-prompt` - Full pipeline: render + call LLM + parse output
- [x] `validate-input` - Validate input against schema
- [ ] `define-prompt` - Programmatic prompt definition (without file)

### Phase 7: Testing
- [x] Set up test framework (fiveam or parachute)
- [x] Unit tests for YAML frontmatter parsing
- [x] Unit tests for Handlebars template rendering
- [x] Unit tests for schema validation
- [ ] Integration tests with sample .prompt files
- [ ] Mock LLM responses for end-to-end tests

### Phase 8: Documentation & Examples
- [x] Write README with installation and usage
- [x] Add example .prompt files
- [x] Document API with docstrings
- [ ] Create tutorial/quickstart guide

## Dependencies to Consider
- `cl-yaml` - YAML parsing
- `yason` or `cl-json` - JSON parsing/generation
- `dexador` or `drakma` - HTTP client for API calls
- `str` or `cl-ppcre` - String manipulation
- `fiveam` or `parachute` - Testing framework

## Example Usage (Target API)
```lisp
;; Load and execute a prompt
(let ((prompt (clprompt:load-prompt "prompts/extract-info.prompt")))
  (clprompt:execute-prompt prompt
    :input '(:text "John Smith is a 35-year-old software engineer.")
    :provider :gemini))

;; Just render the template (for inspection)
(clprompt:render-prompt prompt
  :input '(:text "Some text here"))
```



