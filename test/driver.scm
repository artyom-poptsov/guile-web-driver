(define-module (test driver))

(use-modules
  (ice-9 iconv) (ice-9 match) (ice-9 threads)
  (hdt hdt)
  (web client) (web driver) (web request) (web response) (web server) (web uri))

(test set-web-handler!
  (set-web-handler! (lambda (request body) (values '() "test")))
  (call-with-values
    (lambda () (http-get "http://localhost:8080/"))
    (lambda (response body)
      (assert (equal? 200 (response-code response)))
      (assert (equal? "test" body)))))
  
; Use only one driver, default, to speed up the tests
(hook (close-web-driver))

(define (const-html html)
  (lambda (request body) (values '((content-type . (text/html))) html)))

(test web-driver
  (test current-url
    ; To get current url we do not really need to run a web server
    (navigate-to "http://localhost:8080")
    (assert (equal? "http://localhost:8080/" (current-url))))
  (test element-by-id
    (set-web-handler! (const-html "<html><body><div id='theid'>content</div></body></html>"))
    (navigate-to "http://localhost:8080")
    (assert (element-by-id "theid"))
    (assert (throws-exception (element-by-id "missing"))))
  (test element-by-class-name
    (set-web-handler! (const-html "<html><body><div class='clazz'>xxx</div></body></html>"))
    (navigate-to "http://localhost:8080")
    (assert (element-by-class-name "clazz"))
    (assert (throws-exception (element-by-class-name "missing"))))
  (test elements-by-class-name
    (set-web-handler! 
      (const-html 
        "<html><body><div class='clazz'>one</div><div class='clazz'>two</div></body></html>"))
    (navigate-to "http://localhost:8080")
    (let ((divs (elements-by-class-name "clazz")))
      (assert (list? divs))
      (assert (equal? 2 (length divs)))
      (assert (equal? "one" (text (car divs))))
      (assert (equal? "two" (text (cadr divs))))))
  (test text
    (set-web-handler! (const-html "<html><body>outer <div id='theid'>text</div></body></html>"))
    (navigate-to "http://localhost:8080")
    (assert (equal? "text" (text (element-by-id "theid")))))
  (test attribute
    (set-web-handler! (const-html "<html><body><div id='theid' class='cool'>text</div></body></html>"))
    (navigate-to "http://localhost:8080")
    (assert (equal? "cool" (attribute (element-by-id "theid") "class"))))
  (test click
    (set-web-handler!
      (lambda (request body)
        (values
          '((content-type . (text/html)))
          (match (uri-path (request-uri request))
            ("/" "<html><body><a id='one' href='/one'>one</a></body></html>")
            ("/one/" "<html><body>one</body></html>")
            (else "")))))
    (navigate-to "http://localhost:8080")
    (click (element-by-id "one"))
    (assert (equal? "http://localhost:8080/one" (current-url))))
  (test send-keys
    (set-web-handler!
      (const-html
        "<html><body><form method='get' action='submit'>
           <input id='text' type='text' name='text' />
           <input id='submit' type='submit'/>
         </form></body></html>"))
    (navigate-to "http://localhost:8080")
    (send-keys (element-by-id "text") "keys")
    (click (element-by-id "submit"))
    (assert (equal? "http://localhost:8080/submit?text=keys" (current-url)))))
    
; TODO test cookies

