;;; proxy.scm -- Selenium WebDriver Interceptor Chain.

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

;; This module describes a proxy interceptor chain -- a list of rules that
;; must be applied on intercepted HTTP messages.


;;; Code:

(define-module (web proxy interceptor chain)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:export (chain-select
            chain-run

            rule:type
            rule:field
            rule:action
            rule:parameters))

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
  (reverse (fold (lambda (rule prev)
                   (if (equal? (rule:type rule) type)
                       (cons rule prev)
                       prev))
                 '()
                 chain)))

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
                    (action rule prev-object))
                   (else
                    (error "Unknown action" action))))
                prev-object)))
        object
        chain))

;;; chain.scm ends here.
