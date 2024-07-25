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


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))
