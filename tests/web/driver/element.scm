(use-modules (srfi srfi-64)
             (web driver element))


(define %test-name "web/driver/element.scm")


(test-begin %test-name)

(test-assert "web-driver-element"
  (web-driver-element 'driver %web-driver-element-object-key))

(test-equal "element?: #t"
  #t
  (element? '(web-driver-element driver some-element)))

(test-assert "element-object?"
  (element-object? `((,%web-driver-element-object-key . some-element))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))
