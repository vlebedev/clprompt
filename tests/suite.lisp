;;;; tests/suite.lisp - Test suite definition

(in-package #:clprompt/tests)

;;; ============================================================================
;;; Main Test Suite
;;; ============================================================================

(def-suite clprompt-tests
  :description "Main test suite for clprompt")

;;; ============================================================================
;;; Sub-Suites
;;; ============================================================================

(def-suite parser-tests
  :description "Tests for .prompt file parsing"
  :in clprompt-tests)

(def-suite template-tests
  :description "Tests for Handlebars template engine"
  :in clprompt-tests)

(def-suite schema-tests
  :description "Tests for schema validation"
  :in clprompt-tests)

(def-suite integration-tests
  :description "End-to-end integration tests"
  :in clprompt-tests)

(def-suite define-prompt-tests
  :description "Tests for define-prompt macro"
  :in clprompt-tests)

(def-suite streaming-tests
  :description "Tests for streaming provider APIs"
  :in clprompt-tests)

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-tests ()
  "Run all clprompt tests."
  (run! 'clprompt-tests))
