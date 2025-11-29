;;;; tests/integration-tests.lisp - End-to-end integration tests

(in-package #:clprompt/tests)

(in-suite integration-tests)

;;; ============================================================================
;;; Test Fixtures
;;; ============================================================================

(defparameter *test-examples-dir*
  (asdf:system-relative-pathname :clprompt "examples/")
  "Directory containing example .prompt files.")

;;; ============================================================================
;;; Load Prompt Tests
;;; ============================================================================

(test load-prompt-from-file
  "Loading a .prompt file from disk works."
  (let* ((path (merge-pathnames "extract-info.prompt" *test-examples-dir*))
         (prompt (clprompt:load-prompt path)))
    (is (typep prompt 'clprompt:dotprompt))
    (is (string= "googleai/gemini-1.5-pro" (clprompt:prompt-model prompt)))
    (is (= 0.2 (clprompt::prompt-temperature prompt)))
    (is (eq :json (clprompt:prompt-output-format prompt)))
    (is (not (null (clprompt:prompt-input-schema prompt))))
    (is (not (null (clprompt:prompt-output-schema prompt))))))

(test load-prompt-from-string
  "Loading a .prompt from a string works."
  (let* ((content "---
model: test-model
temperature: 0.5
input:
  schema:
    query: string
---
Answer: {{query}}")
         (prompt (clprompt:load-prompt content)))
    (is (typep prompt 'clprompt:dotprompt))
    (is (string= "test-model" (clprompt:prompt-model prompt)))
    (is (= 0.5 (clprompt::prompt-temperature prompt)))))

;;; ============================================================================
;;; Render Prompt Tests
;;; ============================================================================

(test render-prompt-basic
  "Rendering a prompt with input works."
  (let* ((path (merge-pathnames "extract-info.prompt" *test-examples-dir*))
         (prompt (clprompt:load-prompt path))
         (rendered (clprompt:render-prompt prompt '(:text "John is 30 years old."))))
    (is (stringp rendered))
    (is (search "John is 30 years old." rendered))))

(test render-prompt-validates-input
  "Rendering with :validate t signals error on invalid input."
  (let* ((content "---
model: test-model
input:
  schema:
    name: string
---
Hello {{name}}")
         (prompt (clprompt:load-prompt content)))
    ;; Valid input works
    (is (stringp (clprompt:render-prompt prompt '(:name "Alice") :validate t)))
    ;; Invalid input (number instead of string) signals error
    (signals clprompt:type-validation-error
      (clprompt:render-prompt prompt '(:name 123) :validate t))))

;;; ============================================================================
;;; Execute Prompt Tests
;;; ============================================================================

(test execute-prompt-full-pipeline
  "Execute-prompt runs the full pipeline with mock provider."
  (let* ((content "---
model: test-model
input:
  schema:
    query: string
---
Answer this: {{query}}")
         (prompt (clprompt:load-prompt content))
         (provider (make-mock-provider
                    (list :content "The answer is 42."
                          :usage (list :prompt-tokens 10
                                      :completion-tokens 5
                                      :total-tokens 15))))
         (response (clprompt:execute-prompt prompt '(:query "What is the meaning?")
                                            :provider provider)))
    ;; Check response structure
    (is (string= "The answer is 42." (getf response :content)))
    (is (not (null (getf response :usage))))
    ;; Check that provider received the rendered prompt
    (is (= 1 (request-count provider)))
    (is (search "What is the meaning?" (last-request provider)))))

(test execute-prompt-with-json-output
  "Execute-prompt parses JSON output when format is :json."
  (let* ((content "---
model: test-model
input:
  schema:
    text: string
output:
  format: json
  schema:
    name?: string
    age?: number
---
Extract info from: {{text}}")
         (prompt (clprompt:load-prompt content))
         (provider (make-mock-provider
                    (make-json-response '(("name" . "Alice") ("age" . 30)))))
         (response (clprompt:execute-prompt prompt '(:text "Alice is 30.")
                                            :provider provider)))
    ;; Content should be parsed to a hash-table
    (is (hash-table-p (getf response :content)))
    (is (string= "Alice" (gethash "name" (getf response :content))))
    (is (= 30 (gethash "age" (getf response :content))))))

(test execute-prompt-validates-input
  "Execute-prompt signals error on invalid input."
  (let* ((content "---
model: test-model
input:
  schema:
    count: number
---
Count: {{count}}")
         (prompt (clprompt:load-prompt content))
         (provider (make-mock-provider)))
    ;; String instead of number should fail
    (signals clprompt:type-validation-error
      (clprompt:execute-prompt prompt '(:count "not a number")
                               :provider provider))))

(test execute-prompt-without-provider-signals-error
  "Execute-prompt without provider signals error."
  (let* ((content "---
model: test-model
---
Hello")
         (prompt (clprompt:load-prompt content))
         (clprompt:*default-provider* nil))
    (signals clprompt:provider-error
      (clprompt:execute-prompt prompt '()))))

;;; ============================================================================
;;; JSON Helper Integration Tests
;;; ============================================================================

(test execute-prompt-with-json-helper
  "Templates with {{json var}} work in execute pipeline."
  (let* ((content "---
model: test-model
input:
  schema:
    data: object
---
Process this JSON: {{json data}}")
         (prompt (clprompt:load-prompt content))
         (provider (make-mock-provider
                    (list :content "Processed."
                          :usage nil)))
         (data (alexandria:plist-hash-table '("key" "value") :test 'equal)))
    (clprompt:execute-prompt prompt (list :data data) :provider provider)
    ;; Check that JSON was rendered in the request
    (is (search "{\"key\":\"value\"}" (last-request provider)))))
