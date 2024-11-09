(use-modules (srfi srfi-64)
             (web driver key))



(define %test-name "web/driver/key.scm")


(test-begin %test-name)

(test-equal "key->unicode-char: Cancel -> \uE001"
  "\uE001"
  (key->unicode-char "Cancel"))

(test-equal "key->unicode-char: A -> A"
  "A"
  (key->unicode-char "A"))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))

