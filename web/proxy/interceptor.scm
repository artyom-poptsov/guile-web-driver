(define-module (web proxy interceptor)
  #:use-module (oop goops)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (gnutls)
  #:use-module (web client)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (web proxy common)
  #:use-module (web proxy connection)
  #:export (<proxy-interceptor>
            proxy-interceptor-chain
            proxy-interceptor-tls-session-priorities
            proxy-interceptor-x509-certificate-file
            proxy-interceptor-x509-private-key-file
            proxy-interceptor-run))


;; TODO: Generate the paths to files dynamically.

(define %default-x509-certificate-file
  "cert/cert.pem")

(define %default-x509-private-key-file
  "cert/key.pem")


(define-class <proxy-interceptor> ()
  ;; Each element of a list must have the following format:
  ;;   '(<chain> <field> <action> <parameters>)
  ;;
  ;; Possible chains:
  ;;   request
  ;;   response (not yet)
  ;;
  ;; Possible request/response fields:
  ;;   method
  ;;   uri
  ;;   version
  ;;   headers
  ;;   meta
  ;;   body
  ;;
  ;; Possible actions:
  ;;   'dump
  ;;   'replace
  ;;   <procedure>
  ;;
  ;; The set of parameters depends on the chosen action.  For "dump" action
  ;; its a file name to dump the data into, or one of the following symbols:
  ;;   stderr
  ;;   stdout
  ;;
  ;; For "replace" action the parameter must be the value to set the field to.
  ;;
  ;; Example:
  ;;   '((request headers dump stderr))
  ;;
  ;; <list>
  (chain
   #:init-value   '()
   #:init-keyword #:chain
   #:getter       proxy-interceptor-chain)

  ;; <string>
  (tls-session-priorities
   #:init-value   "NORMAL:+ARCFOUR-128:+CTYPE-X509"
   #:getter       proxy-interceptor-tls-session-priorities)

  ;; The path to a X509 certificate file.
  ;;
  ;; <string>
  (x509-certificate-file
   #:init-value   %default-x509-certificate-file
   #:init-keyword #:x509-certificate-file
   #:getter       proxy-interceptor-x509-certificate-file)

  ;; The path to a X509 private key file.
  ;;
  ;; <string>
  (x509-private-key-file
   #:init-value   %default-x509-private-key-file
   #:init-keyword #:x509-private-key-file
   #:getter       proxy-interceptor-x509-private-key-file)

  ;; INTERNAL.
  (x509-certificate
   #:init-value   #f
   #:getter       proxy-interceptor-x509-certificate
   #:setter       proxy-interceptor-x509-certificate-set!)

  ;; INTERNAL.
  (x509-private-key
   #:init-value   #f
   #:getter       proxy-interceptor-x509-private-key
   #:setter       proxy-interceptor-x509-private-key-set!))



(define (import-key import-proc file)
  "Import a key FILE using a procedure IMPORT-PROC, return the imported key."
  (let* ((raw (get-bytevector-all (open-input-file file))))
    (import-proc raw x509-certificate-format/pem)))

(define-method (proxy-interceptor-import-keys (interceptor <proxy-interceptor>))
  "Load an X509 certificate and a private key from files specified for an
INTERCEPTOR.  Return two values: a X509 certificate and a private key."
  (values (import-key import-x509-certificate
                      (proxy-interceptor-x509-certificate-file interceptor))
          (import-key import-x509-private-key
                      (proxy-interceptor-x509-private-key-file interceptor))))

(define-method (initialize (interceptor <proxy-interceptor>) initargs)
  "The <proxy-interceptor> constructor."
  (next-method)
  (receive (certificate private-key)
      (proxy-interceptor-import-keys interceptor)
    (format (current-error-port) "certificate: ~S~%" certificate)
    (format (current-error-port) "private key: ~S~%" private-key)
    (proxy-interceptor-x509-certificate-set! interceptor certificate)
    (proxy-interceptor-x509-private-key-set! interceptor private-key)))



(define-method (rule:type (rule <list>))
  (list-ref rule 0))

(define-method (rule:field (rule <list>))
  (list-ref rule 1))

(define-method (rule:action (rule <list>))
  (list-ref rule 2))

(define-method (rule:parameters (rule <list>))
  (list-ref rule 3))

(define-method (chain-select (chain <list>) (type <symbol>))
  "Select all the chains of the TYPE from a SCENARIO."
  (fold (lambda (rule prev)
          (if (equal? (rule:type rule) type)
              (cons rule prev)
              prev))
        '()
        chain))

(define-method (chain-run (chain <list>) (field <symbol>) object)
  "Run an interceptor CHAIN for a FIELD on an OBJECT (the field value.)"
  (fold (lambda (rule prev-object)
          (format (current-error-port) "rule: ~a; obj: ~a~%"
                  rule prev-object)
          (let ((current-field (rule:field rule)))
            (if (equal? current-field field)
                (let ((action     (rule:action rule))
                      (parameters (rule:parameters rule)))
                  (cond
                   ((equal? action 'dump)
                    (pretty-print prev-object)
                    prev-object)
                   ((equal? action 'replace)
                    parameters)
                   ((procedure? action)
                    (action chain prev-object))
                   (else
                    (error "Unknown action" action))))
                prev-object)))
        object
        chain))



(define-method (proxy-interceptor-make-session! (interceptor <proxy-interceptor>)
                                                (connection <proxy-connection>))
  (let ((server      (make-session connection-end/server))
        (client-port (proxy-connection-client-port connection))
        (target-port (proxy-connection-target-port connection))
        (pub         (proxy-interceptor-x509-certificate interceptor))
        (sec         (proxy-interceptor-x509-private-key interceptor)))

    (set-session-priorities!
     server
     (proxy-interceptor-tls-session-priorities interceptor))

    ;; Specify the underlying transport socket.
    (set-session-transport-fd! server (fileno client-port))

    ;; Create anonymous credentials.
    (let ((cred (make-certificate-credentials)))
      (set-certificate-credentials-x509-keys! cred
                                              (list pub)
                                              sec)
      (set-session-credentials! server cred))

    ;; Perform the TLS handshake with the client.
    (handshake server)
    (proxy-connection-tls-session-set! connection server)))

(define-method (proxy-interceptor-run (interceptor <proxy-interceptor>)
                                      (connection <proxy-connection>))
  (unless (proxy-connection-tls-session connection)
    (proxy-interceptor-make-session! interceptor connection))

  ;; Receive data over the TLS record layer.
  (let* ((server         (proxy-connection-tls-session connection))
         (origin-request (catch #t
                           (lambda ()
                             (read-request (session-record-port server)))
                           (lambda (key . args)
                             (format (current-error-port)
                                     "ERROR: ~a: ~a~%" key args)
                             #f)))
         (scenario       (proxy-interceptor-chain interceptor)))
    (if origin-request
        (let* ((request-chain  (chain-select scenario 'request))
               (response-chain (chain-select scenario 'response))
               (method
                (if request-chain
                    (chain-run request-chain
                               'method
                               (request-method origin-request))
                    (request-method origin-request)))
               (uri
                (if request-chain
                    (chain-run request-chain
                               'uri
                               (request-uri origin-request))
                    (request-uri origin-request)))
               (version
                (if request-chain
                    (chain-run request-chain
                               'version
                               (request-version origin-request))
                    (request-version origin-request)))
               (headers
                (if request-chain
                    (chain-run request-chain
                               'headers
                               (request-headers origin-request))
                    (request-headers origin-request)))
               (meta
                (if request-chain
                    (chain-run request-chain
                               'meta
                               (request-meta origin-request))
                    (request-meta origin-request)))
               (uri        (build-uri 'https
                                      #:host (proxy-connection-host connection)
                                      #:port (proxy-connection-port connection)
                                      #:path (uri-path uri)
                                      #:query (uri-query uri))))
          (format #t "received the following message: ~a~%"
                  origin-request)
          (format #t "uri: ~a~%"
                  uri)
          (receive (response response-body)
              (http-request uri
                            #:method method
                            #:version version
                            #:headers headers
                            #:decode-body? #f)
            (write-response response (session-record-port server))
            (put-bytevector (session-record-port server)
                            response-body)
            (force-output (session-record-port server))
            (bye server close-request/rdwr)))
        (begin
          (close (proxy-connection-client-port connection))))))

;;; interceptor.scm ends here.
