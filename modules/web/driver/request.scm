(define-module (web driver request)
  #:use-module (json)
  #:use-module (web client)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web driver log)
  #:use-module ((web driver error) #:prefix error:)
  #:use-module (web driver common)
  #:export (request))

(define (request method uri body-scm)
  (log-debug "request: method: ~a uri: ~a"
             method
             uri)
  (let* ((body-string (scm->json-string body-scm))
         (body-bytevector (and body-scm
                               (request-body->bytevector body-string))))
    (call-with-values
        (lambda ()
          (http-request uri #:method method #:body body-bytevector))
      (lambda (response body)
        (let ((value (assoc-ref (json-bytevector->scm body) "value")))
          (log-debug "request: response-code: ~a" (response-code response))
          (if (equal? 200 (response-code response))
              value
              (let ((error (assoc-ref value "error"))
                    (message (assoc-ref value "message")))
                (error:web-driver-error
                 "~a ~a.\nRequest: ~a ~a\nBody: ~a\nError: ~a\nMessage: ~a\n"
                 (response-code response)
                 (response-reason-phrase response)
                 method
                 uri
                 body-string
                 error
                 message))))))))

;;; request.scm ends here.
