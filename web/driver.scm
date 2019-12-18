(define-module (web driver))

(use-modules
  (ice-9 hash-table) (ice-9 iconv) (ice-9 match) (ice-9 popen)
  (json)
  (srfi srfi-1) (srfi srfi-9) (srfi srfi-27)
  (web client) (web request) (web response) (web server))

(define web-server #f)
(define current-handler #f)

(define-public (set-web-handler! handler)
  "Sets the current handler for the testing web server listening on localhost:8080."
  (set! current-handler handler) 
  (if (not web-server)
; Start listening in calling thread, so the client can connect as soon as this procedure returns
    (let ((server-socket (socket PF_INET SOCK_STREAM 0)))
      (setsockopt server-socket SOL_SOCKET SO_REUSEADDR 1)
      (bind server-socket AF_INET INADDR_LOOPBACK 8080)
      (listen server-socket 16)
      (set! web-server #t)
      (call-with-new-thread
        (lambda ()
          (run-server 
            (lambda (request body) (current-handler request body))
            'http
            (list #:socket server-socket)))))))

(define (request method uri body-scm)
  (define body-bytevector (and body-scm (string->bytevector (scm->json-string body-scm) "utf-8")))
  (call-with-values
    (lambda ()
      (http-request uri #:method method #:body body-bytevector))
    (lambda (response body)
      (let ((value (hash-ref (json-string->scm (bytevector->string body "utf-8")) "value")))
        (if (equal? 200 (response-code response))
          value
          (let ((error (hash-ref value "error"))
                (message (hash-ref value "message")))
            (throw 'web-driver-error
              (format #f "Request ~a ~a failed with status ~a ~a.\nError: ~a\nMessage: ~a\n" 
                method uri (response-code response) (response-reason-phrase response) error message))))))))

(define (close-driver-pipe driver-pipe)
; It is possible that the process has already terminated after the session was deleted
  (format driver-pipe "kill $DRIVERPID 2>/dev/null\n")
  (format driver-pipe "wait $DRIVERPID")
  (format driver-pipe "exit\n")
  (close-pipe driver-pipe))

(define (free-listen-port)
  "Find an unused port for server to listen on it"
  (define s (socket PF_INET SOCK_STREAM 0))
  (listen s 1)
  (let ((port (array-ref (getsockname s) 2)))
    (close-port s)
    port))
      
(define-public (open-web-driver)
  "Opens a web driver session. 
   Caller needs to close this session by calling *close-web-driver* when done."
  (let* ((port (free-listen-port))
         (driver-pipe (open-output-pipe "sh"))
         (driver-uri (format #f "http://localhost:~a" port)))
    (format driver-pipe "chromedriver --port=~a >/dev/null &\n" port)
    (format driver-pipe "DRIVERPID=$!\n")
; wait until the new process starts listening
    (find
      (lambda (try)
        (catch #t
          (lambda () (request 'GET (format #f "~a/status" driver-uri) #f) #t)
          (lambda (key . args) (usleep (* 10 1000)) #f)))
      (iota 100))
; start a new session
    (catch #t
      (lambda ()
        (let* ((uri (format #f "~a/session" driver-uri))
               (parameters (json (object ("capabilities" (object)))))
               (response (request 'POST uri parameters))
               (session-id (hash-ref response "sessionId")))
          (list 'web-driver driver-pipe driver-uri session-id)))
      (lambda (key . args)
        (close-driver-pipe driver-pipe)
        (apply throw key args)))))

(define-public (web-driver? object)
  (match object
    (('web-driver driver-pipe driver-uri session-id) #t)
    (else #f)))

(define-public (web-driver-open? driver)
  (match driver
    (('web-driver driver-pipe driver-uri session-id) (not (port-closed? driver-pipe)))))

(define (session-command driver method path body-scm)
  (match driver
    (('web-driver driver-pipe driver-uri session-id)
      (request method (format #f "~a/session/~a~a" driver-uri session-id path) body-scm))))

(define *default-driver* (make-thread-local-fluid))

(define (close driver)
  (match driver
    (('web-driver driver-pipe driver-uri session-id)
      (session-command driver 'DELETE "" #f)
      (close-driver-pipe driver-pipe))))

(define-public (close-web-driver . args)
  (if (null? args)
    (if (fluid-ref *default-driver*)
      (begin
        (close (fluid-ref *default-driver*))
        (fluid-set! *default-driver* #f)))
    (close (car args))))

(define-public (call-with-web-driver proc)
  (define driver (open-web-driver))
  (catch #t
    (lambda () 
      (let ((r (with-fluid* *default-driver* driver (lambda () (proc driver)))))
        (close-web-driver driver) r))
    (lambda args 
      (close-web-driver driver) (apply throw args))))

(define-public (get-default-driver)
  (let ((current (fluid-ref *default-driver*)))
    (if (or (not current) (not (web-driver-open? current)))
      (fluid-set! *default-driver* (open-web-driver))))
  (fluid-ref *default-driver*))

(define-syntax define-public-with-driver
  (syntax-rules ()
    ((define-public-with-driver (proc-name driver args* ...) body* ...)
     (define-public (proc-name . args)
       (let ((proc (lambda* (driver args* ...) body* ...)))
             (if (and (pair? args) (web-driver? (car args)))
               (apply proc args)
               (apply proc (get-default-driver) args)))))))

;;; Navigation

(define-public-with-driver (navigate-to driver url)
  (session-command driver 'POST "/url" (json (object ("url" ,url)))))

(define-public-with-driver (current-url driver) 
  (session-command driver 'GET "/url" #f))

(define-public-with-driver (back driver) 
  (session-command driver 'POST "/back" (json (object))))

(define-public-with-driver (forward driver)
  (session-command driver 'POST "/forward" (json (object))))

(define-public-with-driver (refresh driver)
  (session-command driver 'POST "/refresh" (json (object))))

(define-public-with-driver (title driver)
  (session-command driver 'GET "/title" #f))

;;; Elements

; XXX elements are returned as a json object with a single weird key
; with value of the actual element id/reference
(define (web-driver-element driver element-object)
  (list 'web-driver-element driver (hash-ref element-object "element-6066-11e4-a52e-4f735466cecf")))

(define (element-command element method path body-scm)
  (match element
    (('web-driver-element driver element)
      (session-command driver method (format #f "/element/~a~a" element path) body-scm))))

;;; Finding Elements

(define (find-element driver using value)
  (web-driver-element driver
    (session-command driver 
      'POST "/element" 
      (json (object ("using" ,using) ("value" ,value))))))

(define (find-element-from driver from using value)
  (web-driver-element driver
    (element-command from
      'POST "/element"
      (json (object ("using" ,using) ("value" ,value))))))

(define (find-elements driver using value)
  (map
    (lambda (element-object) (web-driver-element driver element-object))
    (session-command driver 
      'POST "/elements" 
      (json (object ("using" ,using) ("value" ,value))))))

(define (find-elements-from driver from using value)
  (map
    (lambda (element-object) (web-driver-element driver element-object))
    (element-command from 
      'POST "/elements" 
      (json (object ("using" ,using) ("value" ,value))))))

(define-syntax define-finder
  (syntax-rules ()
    ((define-finder element-by elements-by using filter)
     (begin
       (define-public-with-driver (element-by driver value #:key (from #f))
         (if from
           (find-element-from driver from using (filter value))
           (find-element driver using (filter value))))
       (define-public-with-driver (elements-by driver value #:key (from #f))
         (if from
           (find-elements-from driver from using (filter value))
           (find-elements driver using (filter value))))))
    ((define-finder element-by elements-by using)
     (define-finder element-by elements-by using identity))))

(define-finder element-by-css-selector elements-by-css-selector "css selector")

; TODO check that the id and class name are valid
; They should be at least one character and not contain any space characters

(define-finder element-by-id elements-by-id 
  "css selector" (lambda (id) (string-append "#" id)))
  
(define-finder element-by-class-name elements-by-class-name 
  "css selector" (lambda (class-name) (string-append "." class-name)))

(define-finder element-by-tag-name elements-by-tag-name "tag name")

(define-finder element-by-link-text elements-by-link-text "link text")

(define-finder element-by-partial-link-text elements-by-partial-link-text "partial link text")

(define-finder element-by-xpath elements-by-xpath "xpath")

(define-public-with-driver (active-element driver)
  (web-driver-element driver (session-command driver 'GET "/element/active" #f)))

;;; Element State

(define-public (selected? element)
  (element-command element 'GET "/selected" #f))

(define-public (attribute element name)
  (element-command element 'GET (format #f "/attribute/~a" name) #f))

(define-public (property element name)
  (element-command element 'GET (format #f "/property/~a" name) #f))

(define-public (css-value element name)
  (element-command element 'GET (format #f "/css/~a" name) #f))

(define-public (text element)
  (element-command element 'GET "/text" #f))

(define-public (tag-name element)
  (element-command element 'GET "/name" #f))

(define-public (enabled? element)
  (element-command element 'GET "/enabled" #f))

;;; Interacting with elements

(define-public (click element)
  (element-command element 'POST "/click" (json (object))))

(define-public (clear element)
  (element-command element 'POST "/clear" (json (object))))

(define-public (send-keys element text)
  (element-command element 'POST "/value" (json (object ("text" ,text)))))

;;; Cookies

(define-record-type <cookie>
  (make-cookie name value path domain secure http-only expiry same-site)
  cookie?
  (name       cookie-name)
  (value      cookie-value)
  (path       cookie-path)
  (domain     cookie-domain)
  (secure     cookie-secure)
  (http-only  cookie-http-only)
  (expiry     cookie-expire)
  (same-site  cookie-same-site))

(export 
  cookie-name cookie-value cookie-path cookie-domain cookie-secure 
  cookie-http-only cookie-expire cookie-same-site)

(define (parse-cookie hash)
  (make-cookie 
    (hash-ref hash "name") 
    (hash-ref hash "value") 
    (hash-ref hash "path" "/")
    (hash-ref hash "domain")
    (hash-ref hash "secure" #f)
    (hash-ref hash "httpOnly" #f)
    (hash-ref hash "expiry" #f)
    (hash-ref hash "samesite" #f)))

(define-public-with-driver (get-all-cookies driver)
  (map
    parse-cookie
    (session-command driver 'GET "/cookie" #f)))

(define-public-with-driver (get-named-cookie driver name)
  (parse-cookie (session-command driver 'GET (format #f "/cookie/~a" name) #f)))

(define-public-with-driver 
  (add-cookie driver #:key name value path domain secure http-only expiry same-site)
  (let* ((add (lambda (key value) (if value (list (cons key value)) '())))
         (args
           (append 
             (add "name" name) (add "value" value) (add "path" path) (add "domain" domain) 
             (add "secure" secure) (add "httpOnly" http-only) (add "expiry" expiry) 
             (add "samesite" same-site)))
         (hash (alist->hash-table (list (cons "cookie" (alist->hash-table args))))))
    (session-command driver 'POST "/cookie" hash)))

(define-public-with-driver (delete-named-cookie driver name)
  (session-command driver 'DELETE (format #f "/cookie/~a" name) #f))

(define-public-with-driver (delete-all-cookies driver)
  (session-command driver 'DELETE "/cookie" #f))

