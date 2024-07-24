;;; common.scm -- Common Guile-Web-Driver-NG proxy procedures.

;; Copyright (C) 2022-2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains common procedures that are used in different parts of
;; Guile-Web-Driver-NG proxy.


;;; Code:

(define-module (web proxy common)
  #:use-module (oop goops)
  #:use-module (ice-9 threads)
  #:use-module (rnrs bytevectors)
  #:export (object-address/hex-string
            format))


(define (object-address/hex-string object)
  (number->string (object-address object) 16))


;; TODO: This is for debugging.  Implement proper logging instead.
(define mtx (make-mutex 'recursive))
(define (format dest fmt . args)
  (lock-mutex mtx)
  (apply (@@ (guile) format)
         dest
         (string-append ";;; " fmt)
         args)
  (unlock-mutex mtx))

;;; common.scm ends here.
