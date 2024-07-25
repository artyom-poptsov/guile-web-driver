(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (oop goops)
             (web proxy connection)
             (web proxy interceptor)
             (web proxy))


(define %test-name "web/proxy.scm")


(test-begin %test-name)

(test-assert "make <proxy>"
  (make <proxy>))

(test-assert "display"
  (display (make <proxy>)))

(test-assert "proxy?"
  (proxy? (make <proxy>)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))
