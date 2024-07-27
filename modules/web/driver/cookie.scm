;;; driver.scm -- Selenium WebDriver implementation.

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

;; This module contains classes and procedures for working with cookies.


;;; Code:

(define-module (web driver cookie)
  #:use-module (srfi srfi-9)
  #:export (<cookie>
            make-cookie
            cookie-name
            cookie-value
            cookie-path
            cookie-domain
            cookie-secure
            cookie-http-only
            cookie-expire
            cookie-same-site
            parse-cookie))

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

(define (parse-cookie hash)
  (make-cookie
   (assoc-ref hash "name")
   (assoc-ref hash "value")
   (or (assoc-ref hash "path") "/")
   (assoc-ref hash "domain")
   (assoc-ref hash "secure")
   (assoc-ref hash "httpOnly")
   (assoc-ref hash "expiry")
   (assoc-ref hash "samesite")))

;;; cookie.scm ends here.
