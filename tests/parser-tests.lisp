;;;; tests/parser-tests.lisp - Parser tests

(in-package #:clprompt/tests)

(in-suite parser-tests)

;;; ============================================================================
;;; Frontmatter Splitting Tests
;;; ============================================================================

(test split-frontmatter-basic
  "Basic frontmatter splitting works correctly."
  (multiple-value-bind (fm body)
      (clprompt::split-frontmatter
       "---
model: gemini-1.5-pro
---
Hello {{name}}")
    (is (string= "model: gemini-1.5-pro" (string-trim '(#\Newline #\Space) fm)))
    (is (string= "Hello {{name}}" body))))

(test split-frontmatter-no-frontmatter
  "Files without frontmatter return NIL and full content."
  (multiple-value-bind (fm body)
      (clprompt::split-frontmatter "Just a template {{var}}")
    (is (null fm))
    (is (string= "Just a template {{var}}" body))))

(test split-frontmatter-unclosed-signals-error
  "Unclosed frontmatter signals an error."
  (signals clprompt:frontmatter-error
    (clprompt::split-frontmatter "---
model: test
No closing delimiter")))

;;; ============================================================================
;;; Full Parse Tests
;;; ============================================================================

(test parse-complete-prompt
  "Parsing a complete .prompt file works."
  (let ((prompt (clprompt:load-prompt
                 "---
model: googleai/gemini-1.5-pro
temperature: 0.7
---
Hello {{name}}, how are you?")))
    (is (typep prompt 'clprompt:dotprompt))
    (is (string= "googleai/gemini-1.5-pro" (clprompt:prompt-model prompt)))
    (is (= 0.7 (getf (clprompt:prompt-config prompt) :temperature)))))

(test parse-prompt-without-frontmatter
  "Parsing a prompt without frontmatter works."
  (let ((prompt (clprompt:load-prompt "Just {{content}}")))
    (is (typep prompt 'clprompt:dotprompt))
    (is (null (clprompt:prompt-model prompt)))))
