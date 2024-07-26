;;; error.scm -- Selenium WebDriver errors.

;; Copyright (C) 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, version 3.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This module contains Guile-WebDriver-NG errors.


;;; Code:

(define-module (web driver error)
  #:use-module (ice-9 match)
  #:export (not-implemented
            unknown-browser
            invalid-arguments
            web-driver-error))



(define (not-implemented message)
  (throw 'not-implemented message))

(define (unknown-browser browser)
  (throw 'unknown-browser
         (format #f "The browser ~a is not supported." browser)))

(define invalid-arguments
  (case-lambda
    ((message)
     (throw 'invalid-arguments message))
    ((message . args)
     (apply throw 'invalid-arguments message args))))

(define (web-driver-error fmt . args)
  (throw 'web-driver-error (apply format #f fmt args)))

;;; error.scm ends here.
