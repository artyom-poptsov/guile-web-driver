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

;; This module contains some procedures for working with element objects.


;;; Code:

(define-module (web driver element)
  #:use-module (ice-9 match)
  #:export (web-driver-element
            element?
            element-object?

            %web-driver-element-object-key))

;; XXX elements are returned as a json object with a single weird key with
;; value of the actual element id/reference
(define %web-driver-element-object-key
  "element-6066-11e4-a52e-4f735466cecf")


(define (web-driver-element driver element-object)
  (list
   'web-driver-element driver
   (assoc-ref element-object %web-driver-element-object-key)))

(define (element? value)
  (match value
    (('web-driver-element driver element) #t)
    (_ #f)))

(define (element-object? element-object)
  (and
   (list? element-object)
   (assoc-ref element-object %web-driver-element-object-key)))

;;; element.scm ends here.
