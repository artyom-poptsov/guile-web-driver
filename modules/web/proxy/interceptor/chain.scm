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

            replace
            append
            delete

            rule:type
            rule:field
            rule:action
            rule:parameters))


(define-method (rule:type (rule <list>))
  "Get the @code{type} of a @var{rule}."
  (list-ref rule 0))

(define-method (rule:field (rule <list>))
  "Get the @code{field} of a @var{rule}."
  (list-ref rule 1))

(define-method (rule:action (rule <list>))
  "Get the @code{action} of a @var{rule}."
  (list-ref rule 2))

(define-method (rule:parameters (rule <list>))
  "Get the @code{parameters} of a @var{rule}."
  (list-ref rule 3))



(define-method (replace (rule <list>) (old-object <list>))
  "Replace elements from an associative list @var{old-object} with new ones from
the @var{rule} parameters.  Return new object."
  (let ((new-object (rule:parameters rule)))
    (reverse (fold (lambda (old-element prev)
                     (let* ((element-name (car old-element))
                            (new-element  (assq element-name new-object)))
                       (if new-element
                           (cons new-element prev)
                           (cons old-element prev))))
                   '()
                   old-object))))

(define-method (append (rule <list>) (old-object <list>))
  "Append new elements from a @var{rule} to elements of an associative list
@var{old-object}.  Return new object."
  (let ((new-elements (rule:parameters rule)))
    (format (current-error-port) "old: ~S; elem: ~S~%" old-object new-elements)
    ((@@ (guile) append) old-object new-elements)))

(define-method (delete (rule <list>) old-object)
  "Delete elements from an associative list @var{old-object} with keys that match
the keys listen in the @var{rule} parameter.  Return new object."
  (let ((elements-to-delete (rule:parameters rule)))
    (reverse (fold (lambda (element prev)
                     (if (member (car element) elements-to-delete)
                         prev
                         (cons element prev)))
                   '()
                   old-object))))


(define-method (chain-select (chain <list>) (type <symbol>))
  "Select all the chains of the @var{type} from a @var{chain}."
  (reverse (fold (lambda (rule prev)
                   (if (equal? (rule:type rule) type)
                       (cons rule prev)
                       prev))
                 '()
                 chain)))

(define-method (chain-run (chain <list>) (field <symbol>) object)
  "Run an interceptor @var{chain} for a @var{field} on an @var{object} (the field
value.)  Return a forged object or the same @var{object} if no rules was
applied to it."
  (fold (lambda (rule prev-object)
          (let ((current-field (rule:field rule)))
            (if (equal? current-field field)
                (let ((action     (rule:action rule))
                      (parameters (rule:parameters rule)))
                  (cond
                   ((equal? action 'dump)
                    (pretty-print prev-object)
                    prev-object)
                   ((equal? action 'set)
                    parameters)
                   ((equal? action 'replace)
                    (replace rule prev-object))
                   ((equal? action 'append)
                    (append rule prev-object))
                   ((equal? action 'delete)
                    (delete rule prev-object))
                   ((procedure? action)
                    (action rule prev-object))
                   (else
                    (error "Unknown action" action))))
                prev-object)))
        object
        chain))

;;; chain.scm ends here.
