(define-module (web driver common)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:export (hash-table->alist
            to-assoc-list))



(define (hash-table->alist hash)
  (hash-fold (lambda (key value alist) (cons (cons key value) alist)) (list) hash))

(define (to-assoc-list scm)
  (match scm
    ((? list? list) list)
    ((? hash-table? hash) (hash-table->alist hash))
    (#f (list))))

;;; common.scm ends here.
