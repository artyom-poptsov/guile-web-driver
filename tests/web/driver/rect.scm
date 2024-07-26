(use-modules (srfi srfi-64)
             (web driver rect))


(define %test-name "web/driver/rect.scm")


(test-begin %test-name)

(test-assert "make-rect"
  (make-rect 10 20 100 200))

(test-equal "result->rect"
  (make-rect 10 20 100 200)
  (result->rect '(("x"      . 10)
                  ("y"      . 20)
                  ("width"  . 100)
                  ("height" . 200))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))
