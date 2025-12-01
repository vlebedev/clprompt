;;;; package.lisp - Package definition for clprompt

(defpackage #:clprompt
  (:use #:cl)
  (:import-from #:alexandria
                #:if-let
                #:when-let
                #:with-gensyms
                #:once-only
                #:ensure-list
                #:hash-table-alist
                #:alist-hash-table
                #:plist-hash-table)
  (:export
   ;; Conditions
   #:clprompt-error
   #:prompt-parse-error
   #:frontmatter-error
   #:template-syntax-error
   #:validation-error
   #:type-validation-error
   #:missing-required-error
   #:template-error
   #:undefined-variable-error
   #:undefined-helper-error
   #:provider-error
   #:authentication-error
   #:rate-limit-error
   #:schema-error

   ;; Condition accessors
   #:error-message
   #:error-line
   #:error-column
   #:error-source
   #:error-schema
   #:error-value
   #:error-provider
   #:error-status-code

   ;; Restarts
   #:use-default
   #:skip-validation
   #:provide-value

   ;; Core protocols
   #:render
   #:validate
   #:parse
   #:execute
   #:compile-template

   ;; Provider protocol
   #:send-request
   #:send-request-streaming
   #:list-models
   #:provider-name
   #:provider-api-key
   #:provider-model
   #:provider-endpoint
   #:provider-available-p

   ;; Provider utilities
   #:print-gemini-models
   #:print-openai-models

   ;; Schema protocol
   #:schema-type
   #:schema-required-p
   #:schema-description
   #:schema-properties
   #:schema-items

   ;; Template protocol
   #:template-source
   #:template-compiled-p

   ;; Dotprompt class and accessors
   #:dotprompt
   #:prompt-model
   #:prompt-config
   #:prompt-input-schema
   #:prompt-output-schema
   #:prompt-output-format
   #:prompt-template
   #:prompt-metadata

   ;; Schema classes
   #:schema
   #:string-schema
   #:number-schema
   #:boolean-schema
   #:array-schema
   #:object-schema
   #:enum-schema

   ;; Template classes
   #:template
   #:handlebars-template

   ;; Template helpers
   #:register-helper

   ;; Provider classes
   #:provider
   #:gemini-provider
   #:openai-provider

   ;; High-level API
   #:load-prompt
   #:render-prompt
   #:execute-prompt
   #:validate-input
   #:define-prompt
   #:with-prompt

   ;; Configuration
   #:*default-provider*
   #:*default-model*
   #:*template-cache-enabled-p*))
