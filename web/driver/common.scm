(define-module (web driver common)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:export (hash-table->alist
            to-assoc-list
            request-body->bytevector
            fold-null))



(define (hash-table->alist hash)
  (hash-fold (lambda (key value alist) (cons (cons key value) alist)) (list) hash))

(define (to-assoc-list scm)
  (match scm
    ((? list? list) list)
    ((? hash-table? hash) (hash-table->alist hash))
    (#f (list))))

(define (request-body->bytevector body-string)
  "Convert a request BODY-STRING into a bytevector.  Return the bytevector."
  (string->bytevector body-string "utf-8"))

(define (fold-null json)
  (match json
    ('null #f)
    (x x)))

;;; common.scm ends here.
