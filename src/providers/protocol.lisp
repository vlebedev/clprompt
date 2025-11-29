;;;; providers/protocol.lisp - Provider base classes and mixins

(in-package #:clprompt)

;;; ============================================================================
;;; Provider Base Class
;;; ============================================================================

(defclass provider ()
  ()
  (:documentation "Abstract base class for LLM providers."))

;;; ============================================================================
;;; Mixins
;;; ============================================================================

(defclass api-key-mixin ()
  ((api-key :initarg :api-key
            :initform nil
            :reader provider-api-key
            :documentation "API key for authentication."))
  (:documentation "Mixin providing API key authentication."))

(defmethod initialize-instance :after ((provider api-key-mixin) &key)
  "Validate that API key is present or try to get from environment."
  (unless (provider-api-key provider)
    (let ((env-key (provider-env-var-name provider)))
      (when env-key
        (setf (slot-value provider 'api-key)
              (uiop:getenv env-key))))))

(defgeneric provider-env-var-name (provider)
  (:documentation "Return the environment variable name for this provider's API key.")
  (:method ((provider provider)) nil))

;;; ============================================================================
;;; HTTP Request Utilities
;;; ============================================================================

(defun http-post-json (url body &key headers)
  "Send a POST request with JSON body to URL.
Returns (values response-body status-code headers)."
  (handler-case
      (multiple-value-bind (body status response-headers)
          (dex:post url
                    :content (yason:with-output-to-string* ()
                               (yason:encode body))
                    :headers (append '(("Content-Type" . "application/json"))
                                     headers))
        (values (yason:parse body) status response-headers))
    (dex:http-request-failed (e)
      (values nil
              (dex:response-status e)
              nil
              (dex:response-body e)))))

(defun http-get-json (url &key headers)
  "Send a GET request to URL and parse JSON response.
Returns (values response-body status-code headers)."
  (handler-case
      (multiple-value-bind (body status response-headers)
          (dex:get url :headers headers)
        (values (yason:parse body) status response-headers))
    (dex:http-request-failed (e)
      (values nil
              (dex:response-status e)
              nil
              (dex:response-body e)))))

(defun build-auth-header (api-key &key (type :bearer))
  "Build an authorization header with API-KEY."
  (case type
    (:bearer (cons "Authorization" (format nil "Bearer ~A" api-key)))
    (:x-api-key (cons "x-api-key" api-key))
    (t (cons "Authorization" api-key))))

;;; ============================================================================
;;; Default Method Implementations
;;; ============================================================================

(defmethod provider-available-p ((provider provider))
  "Default implementation - check if API key is present."
  (and (typep provider 'api-key-mixin)
       (not (null (provider-api-key provider)))))

;;; ============================================================================
;;; List Models Protocol
;;; ============================================================================

(defgeneric list-models (provider &key)
  (:documentation "List available models from PROVIDER.
Returns a list of plists, each containing at least :NAME and :DISPLAY-NAME.
May include additional properties like :DESCRIPTION, :INPUT-TOKEN-LIMIT, etc."))
