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
  #:use-module (web proxy common)
  #:use-module (web proxy config)
  #:use-module (web proxy connection)
  #:use-module (web proxy interceptor chain)
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
    (catch 'gnutls-error
      (lambda ()
        (handshake server))
      (lambda (key . args)
        (format (current-error-port) "ERROR: ~a: ~a~%" key args)))
    (proxy-connection-tls-session-set! connection server)))

(define-method (chain-run (chain <top>)
                          (field <symbol>)
                          (accessor <procedure>)
                          (message <top>))
  "Run an interceptor CHAIN for a given FIELD.  Return forged field or the
original field if a CHAIN is #f."
  (if chain
      (chain-run chain field (accessor message))
      (accessor message)))


(define-method (proxy-interceptor-run (interceptor <proxy-interceptor>)
                                      (connection <proxy-connection>))
  (unless (proxy-connection-tls-session connection)
    (proxy-interceptor-make-session! interceptor connection))

  ;; Receive data over the TLS record layer.
  (let* ((server  (proxy-connection-tls-session connection))
         (request (catch #t
                    (lambda ()
                      (read-request (session-record-port server)))
                    (lambda (key . args)
                      (format (current-error-port)
                              "ERROR: ~a: ~a~%" key args)
                      #f)))
         (scenario (proxy-interceptor-chain interceptor)))
    (if request
        (let* ((request-chain  (chain-select scenario 'request))
               (response-chain (chain-select scenario 'response))
               (method  (chain-run request-chain 'method request-method request))
               (uri     (chain-run request-chain 'uri request-uri request))
               (version (chain-run request-chain 'version request-version request))
               (headers (chain-run request-chain 'headers request-headers request))
               (meta    (chain-run request-chain 'meta    request-meta request))
               (uri        (build-uri 'https
                                      #:host (proxy-connection-host connection)
                                      #:port (proxy-connection-port connection)
                                      #:path (uri-path uri)
                                      #:query (uri-query uri))))
          (format #t "received the following message: ~a~%"
                  request)
          (format #t "uri: ~a~%"
                  uri)
          (receive (response response-body)
              (http-request uri
                            #:method method
                            #:version version
                            #:headers headers
                            #:decode-body? #f)
            (let* ((version
                    (chain-run response-chain
                               'version
                               response-version
                               response))
                   (code
                    (chain-run response-chain
                               'code
                               response-code
                               response))
                   (reason-phrase
                    (chain-run response-chain
                               'reason-phrase
                               response-reason-phrase
                               response))
                   (headers
                    (chain-run response-chain
                               'headers
                               response-headers
                               response))
                   (forged-response (build-response
                                     #:version version
                                     #:code    code
                                     #:reason-phrase reason-phrase
                                     #:headers       headers
                                     #:validate-headers? #f)))
              (write-response forged-response (session-record-port server))
              (put-bytevector (session-record-port server)
                              response-body)
              (force-output (session-record-port server))
              (bye server close-request/rdwr))))
        (begin
          (close (proxy-connection-client-port connection))))))

;;; interceptor.scm ends here.
