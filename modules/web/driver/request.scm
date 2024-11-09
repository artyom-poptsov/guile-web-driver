;;; request.scm -- Guile-WebDriver-NG HTTP requests.

;; Copyright (C) 2019-2024 Michal Herko <michal.herko@disroot.org>
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

;; This module defines HTTP request-related procedures for working with
;; Selenium API.


;;; Code:

(define-module (web driver request)
  #:use-module (json)
  #:use-module (web client)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web driver log)
  #:use-module ((web driver error) #:prefix error:)
  #:use-module (web driver common)
  #:export (request

            make-session-uri
            make-status-uri
            make-element-uri
            make-attribute-uri
            make-property-uri
            make-cookie-uri
            make-css-uri))



(define make-session-uri
  (case-lambda
    ((driver-uri)
     (format #f "~a/session" driver-uri))
    ((driver-uri session-id path)
     (format #f "~a/session/~a~a" driver-uri session-id path))))

(define (make-status-uri driver-uri)
  (format #f "~a/status" driver-uri))

(define (make-element-uri element path)
  (format #f "/element/~a~a" element path))

(define (make-attribute-uri name)
  (format #f "/attribute/~a" name))

(define (make-property-uri name)
  (format #f "/property/~a" name))

(define (make-cookie-uri name)
  (format #f "/cookie/~a" name))

(define (make-css-uri name)
  (format #f "/css/~a" name))


(define (request method uri body-scm)
  (log-debug "request: method: ~a uri: ~a"
             method
             uri)
  (let* ((body-string (scm->json-string body-scm))
         (body-bytevector (and body-scm
                               (request-body->bytevector body-string))))
    (call-with-values
        (lambda ()
          (http-request uri #:method method #:body body-bytevector))
      (lambda (response body)
        (let ((value (assoc-ref (json-bytevector->scm body) "value")))
          (log-debug "request: response-code: ~a" (response-code response))
          (if (equal? 200 (response-code response))
              value
              (let ((error (assoc-ref value "error"))
                    (message (assoc-ref value "message")))
                (error:web-driver-error
                 "~a ~a.\nRequest: ~a ~a\nBody: ~a\nError: ~a\nMessage: ~a\n"
                 (response-code response)
                 (response-reason-phrase response)
                 method
                 uri
                 body-string
                 error
                 message))))))))

;;; request.scm ends here.
