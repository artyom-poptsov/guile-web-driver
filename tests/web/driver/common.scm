(use-modules (srfi srfi-64)
             (ice-9 hash-table)
             (web driver common))


(define %test-name "web/driver/common.scm")


(test-begin %test-name)

(test-assert "to-assoc-list: list"
  (to-assoc-list '((a . b))))

(test-equal "to-assoc-list: hash table"
  '((a . 1)
    (b . 2))
  (let ((ht (make-hash-table)))
    (hash-set! ht 'a 1)
    (hash-set! ht 'b 2)
    (to-assoc-list ht)))

(test-equal "request-body->bytevector"
  #vu8(104 101 108 108 111)
  (request-body->bytevector "hello"))

(test-equal "bytevector->utf-8"
  "hello"
  (bytevector->utf-8 #vu8(104 101 108 108 111)))

(test-equal "json-bytevector->scm"
  '(("key" . "value"))
  (json-bytevector->scm
   #vu8(123 34 107 101 121 34 58 34 118 97 108 117 101 34 125)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))
