;;; javascript.scm -- Guile-WebDriver-NG JavaScript operations.

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

;; This module contains procedures for working with JavaScript in
;; Guile-WebDriver-NG.


;;; Code:

(define-module (web driver javascript)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (web driver element)
  #:export (scm->javascript
            javascript->scm))

(define (scm->javascript value)
  (match value
    (#t #t)
    (#f #f)
    (#nil 'null)
    ((? number? n) n)
    ((? string? s) s)
    (('web-driver-element driver handle)
     `((,%web-driver-element-object-key . ,handle)))
    ((? list? l) (list->vector (map scm->javascript l)))))

(define (javascript->scm driver value)
  (match value
    (#t #t)
    (#f #f)
    (#nil #nil)
    ('null #nil)
    ((? number? n) n)
    ((? string? s) s)
    ((? element-object? r) (web-driver-element driver r))
    ((? vector? v)
     (map (lambda (value) (javascript->scm driver value))
          (vector->list v)))
    ((? list? l)
     (alist->hash-table
      (map (lambda (key . value) (cons key (javascript->scm driver value)))
           l)))))

;;; javascript.scm ends here.
