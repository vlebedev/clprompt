;;;; clprompt.asd - ASDF system definition for clprompt

(asdf:defsystem #:clprompt
  :description "Common Lisp implementation of Google's dotprompt format"
  :author "Vladimir Lebedev <valebedev@hey.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/vlebedev/clprompt"
  :bug-tracker "https://github.com/vlebedev/clprompt/issues"
  :source-control (:git "https://github.com/vlebedev/clprompt.git")
  :depends-on (#:alexandria
               #:cl-ppcre
               #:cl-yaml
               #:yason
               #:dexador
               #:str)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "protocol")
               (:file "model")
               (:file "parser")
               (:file "template")
               (:file "schema")
               (:module "providers"
                :serial t
                :components ((:file "protocol")
                             (:file "gemini")
                             (:file "openai")))
               (:file "execute"))
  :in-order-to ((test-op (test-op #:clprompt/tests))))

(asdf:defsystem #:clprompt/tests
  :description "Test suite for clprompt"
  :depends-on (#:clprompt
               #:fiveam)
  :serial t
  :pathname "tests"
  :components ((:file "package")
               (:file "suite")
               (:file "parser-tests")
               (:file "template-tests")
               (:file "schema-tests")
               (:file "mock-provider")
               (:file "integration-tests"))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :clprompt-tests :clprompt/tests))))
