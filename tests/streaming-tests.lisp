;;;; tests/streaming-tests.lisp - Streaming provider tests

(in-package #:clprompt/tests)

(in-suite streaming-tests)

(defun make-sse-stream (&rest events)
  "Create an SSE stream from event strings, properly separated by blank lines."
  (make-string-input-stream
   (with-output-to-string (s)
     (dolist (event events)
       (format s "data: ~A~%~%" event)))))

(test openai-streaming-collects-chunks
  (let* ((stream (make-sse-stream
                  "{\"choices\":[{\"delta\":{\"content\":\"Hel\"}}]}"
                  "{\"choices\":[{\"delta\":{\"content\":\"lo\"},\"finish_reason\":null}]}"
                  "{\"choices\":[{\"delta\":{},\"finish_reason\":\"stop\"}]}"
                  "[DONE]"))
         (tokens '())
         (response (clprompt::openai-streaming-response
                    stream
                    (lambda (chunk) (push chunk tokens)))))
    (is (string= "Hello" (getf response :content)))
    (is (string= "stop" (getf response :finish-reason)))
    (is (equal '("lo" "Hel") tokens))))

(test gemini-streaming-collects-chunks-and-usage
  (let* ((stream (make-sse-stream
                  "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Hi\"}]},\"finishReason\":null}]}"
                  (concatenate 'string
                               "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"!\"}]},\"finishReason\":\"STOP\"}],"
                               "\"usageMetadata\":{\"promptTokenCount\":1,\"candidatesTokenCount\":2,\"totalTokenCount\":3}}")))
         (tokens '())
         (response (clprompt::gemini-streaming-response
                    stream
                    (lambda (chunk) (push chunk tokens)))))
    (is (string= "Hi!" (getf response :content)))
    (is (string= "STOP" (getf response :finish-reason)))
    (is (equal '("!" "Hi") tokens))
    (let ((usage (getf response :usage)))
      (is (= 1 (getf usage :prompt-tokens)))
      (is (= 2 (getf usage :completion-tokens)))
      (is (= 3 (getf usage :total-tokens))))))

(test mock-provider-streams-canned-chunks
  (let* ((provider (make-mock-provider (list "foo" "bar")))
         (chunks '())
         (response (clprompt:send-request-streaming provider "ignored"
                                                    :on-token (lambda (chunk)
                                                                (push chunk chunks)))))
    (is (string= "foobar" (getf response :content)))
    (is (equal '("bar" "foo") chunks))
    (is (= 1 (request-count provider)))))
