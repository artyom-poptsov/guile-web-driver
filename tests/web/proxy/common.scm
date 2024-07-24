(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (web proxy common))


(test-begin "web/proxy/common")

(test-assert "object-address/hex-string"
  (object-address/hex-string "test"))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "web/proxy/common")

(exit (zero? exit-status))
