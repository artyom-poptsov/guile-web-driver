;;; proxy.scm -- Selenium WebDriver Interceptor.

;; Copyright (C) 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This module describes a proxy interceptor that can be used to intercept
;; HTTPS traffic between a client and a server (given that a client is allowed
;; to accept forged X509 certificate. of course.)
;;
;; Usage example:
;;   (let* ((interceptor
;;           (make <proxy-interceptor>
;;              #:chain '((request headers dump stderr)
;;                        (request headers replace
;;                                 ((host "example.com" . #f)
;;                                  (user-agent . "curl/8.5.0")
;;                                  (accept (*/*)))))))
;;          (proxy (make <proxy>
;;                   #:port 8081
;;                   #:interceptor interceptor)))
;;     (proxy-start! proxy)
;;     (while #t (sleep 5)))


;;; Code:

(define-module (web proxy interceptor)
  #:use-module (oop goops)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (rnrs bytevectors)
  #:use-module (gnutls)
  #:use-module (web http)
  #:use-module (web client)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (web driver log)
  #:use-module (web proxy common)
  #:use-module (web proxy config)
  #:use-module (web proxy connection)
  #:use-module (web proxy interceptor chain)
  #:use-module (web proxy interceptor forger)
  #:export (<proxy-interceptor>
            proxy-interceptor-chain
            proxy-interceptor-tls-session-priorities
            proxy-interceptor-x509-certificate-file
            proxy-interceptor-x509-private-key-file
            proxy-interceptor-run))


(define-class <proxy-interceptor> ()
  ;; Each element of a list must have the following format:
  ;;   '(<chain> <field> <action> <parameters>)
  ;;
  ;; Possible chains:
  ;;   request
  ;;   response
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
  ;;   'set
  ;;   'replace
  ;;   'append
  ;;   'delete
  ;;   <procedure>
  ;;
  ;; The set of parameters depends on the chosen action.  For "dump" action
  ;; its a file name to dump the data into, or one of the following symbols:
  ;;   stderr
  ;;   stdout
  ;;
  ;; For "set" action the parameter must be the value to set the field to.
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
   #:init-keyword #:tls-session-priorities
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



(define-method (%display (interceptor <proxy-interceptor>) (port <port>))
  (format port
          "#<proxy-interceptor certificate: ~a key: ~a chain length: ~a ~a>"
          (proxy-interceptor-x509-certificate-file interceptor)
          (proxy-interceptor-x509-private-key-file interceptor)
          (length (proxy-interceptor-chain interceptor))
          (object-address/hex-string interceptor)))

(define-method (display (interceptor <proxy-interceptor>) (port <port>))
  (%display interceptor port))

(define-method (write (interceptor <proxy-interceptor>) (port <port>))
  (%display interceptor port))



(define (import-key import-proc file)
  "Import a key @var{file} using a procedure @var{import-proc}, return the
imported key."
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
    (log-info "<proxy-interceptor>: certificate: ~S~%" certificate)
    (log-info "<proxy-interceptor>: private key: ~S~%" private-key)
    (proxy-interceptor-x509-certificate-set! interceptor certificate)
    (proxy-interceptor-x509-private-key-set! interceptor private-key)))



(define-method (proxy-interceptor-make-session! (interceptor <proxy-interceptor>)
                                                (connection <proxy-connection>))
  "Make a new TLS session for an @var{interceptor} and a @var{connection}.
Return value is undefined."
  (let ((server      (make-session connection-end/server))
        (client-port (proxy-connection-client-port connection))
        (target-port (proxy-connection-target-port connection))
        (pub         (proxy-interceptor-x509-certificate interceptor))
        (sec         (proxy-interceptor-x509-private-key interceptor)))

    (log-info "proxy-interceptor-make-session!: ~a: ~a"
              interceptor
              connection)

    (let ((priorities (proxy-interceptor-tls-session-priorities interceptor)))
      (log-debug "proxy-interceptor-make-session!: priorities: ~a"
                 priorities)
      (set-session-priorities! server priorities))

    (log-debug "proxy-interceptor-make-session!: set-session-transport-fd!: ~a"
               (fileno client-port))
    ;; Specify the underlying transport socket.
    (set-session-transport-fd! server (fileno client-port))

    (log-debug
     "proxy-interceptor-make-session!: make-certificate-credentials...")
    ;; Create anonymous credentials.
    (let ((cred (make-certificate-credentials)))
      (log-debug
       "proxy-interceptor-make-session!: make-certificate-credentials... done")
      (set-certificate-credentials-x509-keys! cred
                                              (list pub)
                                              sec)
      (log-debug
       "proxy-interceptor-make-session!: set-session-credentials!: ~a"
       cred)
      (set-session-credentials! server cred))

    ;; Perform the TLS handshake with the client.
    (catch 'gnutls-error
      (lambda ()
        (log-debug
         "proxy-interceptor-make-session!: handshake with ~a ..."
         server)
        (handshake server)
        (log-debug
         "proxy-interceptor-make-session!: handshake with ~a ... done"
         server)
        (proxy-connection-tls-session-set! connection server))
      (lambda (key . args)
        (log-error "proxy-interceptor-make-session!: ~a: ~a~%"
                   key
                   args)
        (log-debug "proxy-interceptor-make-session!: closing ports...")
        (close client-port)
        (close target-port)
        (log-debug "proxy-interceptor-make-session!: closing ports... done")
        (proxy-connection-tls-session-set! connection #f)))))

(define-method (chain-run (chain <top>)
                          (field <symbol>)
                          (accessor <procedure>)
                          (message <top>))
  "Run an interceptor @var{chain} for a given @var{field} of a @var{message}
object.  The @var{field} value is accessed using a provided @var{accessor}
procedure.  Return a forged field or the original field if a @var{chain} is
@code{#f}."
  (if chain
      (chain-run chain field (accessor message))
      (accessor message)))

(define-method (proxy-interceptor-run (interceptor <proxy-interceptor>)
                                      (connection <proxy-connection>)
                                      request
                                      body)
  (let* ((chain          (proxy-interceptor-chain interceptor))
         (forged-request (forge-request chain request body))
         (uri            (forged-message-uri forged-request))
         (uri (build-uri 'https
                         #:host  (proxy-connection-host connection)
                         #:port  (proxy-connection-port connection)
                         #:path  (uri-path uri)
                         #:query (uri-query uri))))
    (log-debug "proxy-interceptor-run: forged request: method: ~a; headers: ~a"
               (forged-message-method forged-request)
               (forged-message-headers forged-request))
    (log-debug "proxy-interceptor-run: uri: ~a" uri)
    (log-debug "proxy-interceptor-run: chain: ~S" chain)
    (receive (response response-body)
        (http-request uri
                      #:body (forged-message-body forged-request)
                      #:method (forged-message-method forged-request)
                      #:version (forged-message-version forged-request)
                      #:headers (forged-message-headers forged-request)
                      #:port (proxy-connection-target-port connection)
                      #:keep-alive? #t
                      #:decode-body? #f)
      (log-debug "proxy-interceptor-run: original response: ~S"
                 response)
      (let* ((forged-response (forge-response chain response response-body))
             (client-socket   (proxy-connection-client-port connection))
             (response (build-response
                        #:version (forged-message-version forged-response)
                        #:code    (forged-message-code forged-response)
                        #:reason-phrase (forged-message-reason-phrase forged-response)
                        #:headers       (forged-message-headers forged-response)
                        #:validate-headers? #f
                        #:port client-socket)))
        (log-debug "proxy-interceptor-run: forged response: ~S"
                   response)
        (catch #t
          (lambda ()
            (write-response response client-socket)
            (force-output client-socket)
            (when (forged-message-body forged-response)
              (write-response-body response
                                   (forged-message-body forged-response))
              (force-output client-socket)))
          (lambda (key . args)
            (log-error "proxy-interceptor-run: ~a: ~a" key args)))))))


(define-method (proxy-interceptor-run (interceptor <proxy-interceptor>)
                                      (connection <proxy-connection>))
  "Intercept traffic flowing through a @var{connection} using an
@var{interceptor}.  Return value is undefined."
  (unless (proxy-connection-tls-session connection)
    (log-debug "proxy-interceptor-run: Creating a new TLS session...")
    (proxy-interceptor-make-session! interceptor connection)
    (log-debug "proxy-interceptor-run: Creating a new TLS session... done"))

  (let ((server (proxy-connection-tls-session connection)))
    (log-debug "proxy-interceptor-run: TLS session: ~a" server)
    (when server
      ;; Receive data over the TLS record layer.
      (let* ((request (catch #t
                        (lambda ()
                          (log-debug "proxy-interceptor-run: Reading request from ~a"
                                     (session-record-port server))
                          (read-request (session-record-port server)))
                        (lambda (key . args)
                          (log-error "proxy-interceptor-run: ~a: ~a~%"
                                     key
                                     args)
                          #f)))
             (body     (and request
                            (read-request-body request)))
             (scenario (proxy-interceptor-chain interceptor)))
        (log-debug "proxy-interceptor-run: received a request: ~a"
                   request)
        (if request
            (let* ((forged-request (forge-request scenario request body))
                   (uri (forged-message-uri forged-request))
                   (uri (build-uri 'https
                                   #:host (proxy-connection-host connection)
                                   #:port (proxy-connection-port connection)
                                   #:path (uri-path uri)
                                   #:query (uri-query uri))))
              (log-debug "proxy-interceptor-run: forged request: method: ~a; headers: ~a"
                         (forged-message-method forged-request)
                         (forged-message-headers forged-request))
              (log-debug "proxy-interceptor-run: uri: ~a" uri)
              (log-debug "proxy-interceptor-run: chain: ~S" scenario)
              (receive (response response-body)
                  (http-request uri
                                #:body (forged-message-body forged-request)
                                #:method (forged-message-method forged-request)
                                #:version (forged-message-version forged-request)
                                #:headers (forged-message-headers forged-request)
                                #:keep-alive? #t
                                #:decode-body? #f)
                (log-debug "proxy-interceptor-run: original response: ~S"
                           response)
                (let* ((forged-response (forge-response scenario
                                                        response
                                                        response-body))
                       (response (build-response
                                  #:version (forged-message-version forged-response)
                                  #:code    (forged-message-code forged-response)
                                  #:reason-phrase (forged-message-reason-phrase forged-response)
                                  #:headers       (forged-message-headers forged-response)
                                  #:validate-headers? #f
                                  #:port (session-record-port server))))
                  (log-debug "proxy-interceptor-run: forged response: ~S"
                             response)
                  (catch #t
                    (lambda ()
                      (write-response response (session-record-port server))
                      (force-output (session-record-port server))
                      (when (forged-message-body forged-response)
                        (write-response-body response
                                             (forged-message-body forged-response))
                        (force-output (session-record-port server))
                        (bye server close-request/rdwr)))
                    (lambda (key . args)
                      (close (session-record-port server))
                      (proxy-connection-tls-session-set! connection #f)
                      (log-error "proxy-interceptor-run: ~a: ~a" key args))))))
            (begin
              (close (proxy-connection-client-port connection))))))))

;;; interceptor.scm ends here.
