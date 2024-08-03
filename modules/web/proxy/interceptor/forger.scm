;;; forger.scm -- Request/response forger.

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

;; This module describes a message forger for an intercepting proxy.


;;; Code:


(define-module (web proxy interceptor forger)
  #:use-module (oop goops)
  #:use-module (web proxy interceptor chain)
  #:use-module (web request)
  #:use-module (web response)
  #:export (<forged-message>
            <forged-request>
            <forged-response>
            forged-message-headers
            forged-message-version
            forged-message-body
            forged-message-method
            forged-message-uri
            forged-message-meta
            forged-message-code
            forged-message-reason-phrase
            forge-request
            forge-response))



(define-class <forged-message> ()
  (headers
   #:init-value   '()
   #:init-keyword #:headers
   #:getter       forged-message-headers)

  (version
   #:init-value   #f
   #:init-keyword #:version
   #:getter       forged-message-version)

  (body
   #:init-value   #f
   #:init-keyword #:body
   #:getter       forged-message-body))

(define-class <forged-request> (<forged-message>)
  (method
   #:init-value   #f
   #:init-keyword #:method
   #:getter       forged-message-method)

  (uri
   #:init-value   #f
   #:init-keyword #:uri
   #:getter       forged-message-uri)

  (meta
   #:init-value   #f
   #:init-keyword #:meta
   #:getter       forged-message-meta))

(define-class <forged-response> (<forged-message>)
  (code
   #:init-value   #f
   #:init-keyword #:code
   #:getter       forged-message-code)

  (reason-phrase
   #:init-value   ""
   #:init-keyword #:reason-phrase
   #:getter       forged-message-reason-phrase))



(define-method (forge-request (chain <list>) original-request body)
  (let ((request-chain (chain-select chain 'request)))
    (make <forged-request>
      #:method  (chain-run request-chain
                           'method
                           request-method
                           original-request)
      #:uri     (chain-run request-chain
                           'uri
                           request-uri
                           original-request)
      #:version (chain-run request-chain
                           'version
                           request-version
                           original-request)
      #:headers (chain-run request-chain
                           'headers
                           request-headers
                           original-request)
      #:meta    (chain-run request-chain
                           'meta
                           request-meta
                           original-request)
      #:body    (chain-run request-chain
                           'body
                           body))))

(define-method (forge-response (chain <list>) original-response body)
  (let ((response-chain (chain-select chain 'response)))
    (make <forged-response>
      #:version      (chain-run response-chain
                                'version
                                response-version
                                original-response)
      #:code          (chain-run response-chain
                                 'code
                                 response-code
                                 original-response)
      #:reason-phrase (chain-run response-chain
                                 'reason-phrase
                                 response-reason-phrase
                                 original-response)
      #:headers       (chain-run response-chain
                                 'headers
                                 response-headers
                                 original-response)
      #:body          (chain-run response-chain
                                 'body body))))

;;; forger.scm ends here.

