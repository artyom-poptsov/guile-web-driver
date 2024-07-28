(define-module (web driver common)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:export (hash-table->alist
            to-assoc-list
            request-body->bytevector
            fold-null

            define-with-docs
            define-class-with-docs))


;;; Taken from (scheme documentation).

(define-macro (define-macro-with-docs name-and-args docs . body)
  "Define a macro with documentation."
  `(define-macro ,name-and-args ,docs ,@body))

(define-macro-with-docs (define-with-docs sym docs val)
  "Define a variable with documentation."
  `(begin
     (define ,sym ,val)
     (set-object-property! ,sym 'documentation ,docs)
     *unspecified*))

(define-macro-with-docs (define-class-with-docs name supers docs . slots)
  "Define a class with documentation."
  `(begin
     (define-class ,name ,supers ,@slots)
     (set-object-property! ,name 'documentation ,docs)
     (if #f #f)))



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
