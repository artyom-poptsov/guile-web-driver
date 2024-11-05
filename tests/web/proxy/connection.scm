(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (web proxy connection))


(define %test-name "web/proxy/connection")


(test-begin %test-name)

(test-assert "make <proxy-connection>"
  (make <proxy-connection>))

(test-assert "display"
  (with-output-to-string
    (lambda ()
      (display (make <proxy-connection>)))))

(test-equal "proxy-connection-client-port"
  (current-error-port)
  (proxy-connection-client-port
   (make <proxy-connection>
     #:client `(,(current-error-port) . #()))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))
