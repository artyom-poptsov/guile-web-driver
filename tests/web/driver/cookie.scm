(use-modules (srfi srfi-64)
             (web driver common)
             (web driver cookie))


(define %test-name "web/driver/cookie.scm")


(test-begin %test-name)

(test-assert "make-rect"
  (let ((name      "test")
        (value     "value")
        (path      "/")
        (domain    "example.org")
        (secure    #f)
        (http-only #t)
        (expiry    12345)
        (same-site "samesite"))
  (make-cookie name value path domain secure http-only expiry same-site)))

(test-assert "parse-cookie"
  (let ((ht (make-hash-table 8)))
    (hash-set! ht "name" "test")
    (hash-set! ht "value" "value")
    (hash-set! ht "path"  "/index.html")
    (hash-set! ht "domain" "example.org")
    (hash-set! ht "secure"   #f)
    (hash-set! ht "httpOnly" #t)
    (hash-set! ht "expiry"   12345)
    (hash-set! ht "samesite" "samesite")
    (parse-cookie (hash-table->alist ht))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))
