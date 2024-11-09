(define-module (web driver common)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (json)
  #:export (hash-table->alist
            to-assoc-list
            request-body->bytevector
            bytevector->utf-8
            json-bytevector->scm
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

(define (bytevector->utf-8 bv)
  (bytevector->string bv "utf-8"))

(define (json-bytevector->scm bv)
  (json-string->scm (bytevector->utf-8 bv)))

(define (fold-null json)
  (match json
    ('null #f)
    (x x)))

;;; common.scm ends here.
