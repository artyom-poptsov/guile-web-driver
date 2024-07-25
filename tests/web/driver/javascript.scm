(use-modules (srfi srfi-64)
             (web driver element)
             (web driver javascript))


(define %test-name "web/driver/javascript.scm")


(test-begin %test-name)


(test-equal "scm->javascript: #t"
  #t
  (scm->javascript #t))

(test-equal "scm->javascript: #f"
  #f
  (scm->javascript #f))

(test-equal "scm->javascript: #nil"
  'null
  (scm->javascript #nil))

(test-equal "scm->javascript: <number>"
  42
  (scm->javascript 42))

(test-equal "scm->javascript: <string>"
  "hello"
  (scm->javascript "hello"))

(test-equal "scm->javascript: web-driver-element"
  `((,%web-driver-element-object-key . "test"))
  (scm->javascript '(web-driver-element driver "test")))

(test-equal "scm->javascript: <list>"
  #(42 "is" "the" "answer")
  (scm->javascript '(42 "is" "the" "answer")))



(test-equal "javascript->scm: #t"
  #t
  (javascript->scm 'driver #t))

(test-equal "javascript->scm: #f"
  #f
  (javascript->scm 'driver #f))

(test-equal "javascript->scm: #nil"
  #nil
  (javascript->scm 'driver #nil))

(test-equal "javascript->scm: 'null"
  #nil
  (javascript->scm 'driver 'null))

(test-equal "javascript->scm: <number>"
  42
  (javascript->scm 'driver 42))

(test-equal "javascript->scm: <vector>"
  '(1 2 3 4)
  (javascript->scm 'driver #(1 2 3 4)))

(test-assert "javascript->scm: <list>"
  (javascript->scm 'driver '((key1 . 1) (key2 . 2))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))
