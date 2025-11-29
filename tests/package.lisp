;;;; tests/package.lisp - Test package definition

(defpackage #:clprompt/tests
  (:use #:cl #:clprompt #:fiveam)
  (:export #:clprompt-tests
           #:run-tests))
