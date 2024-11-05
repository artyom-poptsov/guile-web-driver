(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (web proxy connection)
             (web proxy interceptor))


(define %test-name "web/proxy/interceptor")


(test-begin %test-name)

(define %certificate
  (format #f "~a/cert/cert.pem" (getenv "abs_top_srcdir")))
(define %key
  (format #f "~a/cert/key.pem" (getenv "abs_top_srcdir")))

(test-assert "make <proxy-interceptor>"
  (make <proxy-interceptor>
    #:x509-certificate-file %certificate
    #:x509-private-key-file %key))

(test-assert "display"
  (with-output-to-string
    (lambda ()
      (display (make <proxy-interceptor>
                 #:x509-certificate-file %certificate
                 #:x509-private-key-file %key)))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))
