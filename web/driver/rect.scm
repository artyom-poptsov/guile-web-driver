;;; rect.scm -- Guile-WebDriver-NG rectangle record.

;; Copyright (C) 2019-2024 Michal Herko <michal.herko@disroot.org>
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

;; This module defines <rect> (rectangle) record and related methods.


;;; Code:

(define-module (web driver rect)
  #:use-module (srfi srfi-9)
  #:export (<rect>
            make-rect
            rect?
            rect-x
            rect-y
            rect-width
            rect-height))



(define-record-type <rect>
  (make-rect x y width height)
  rect?
  (x       rect-x)
  (y       rect-y)
  (width   rect-width)
  (height  rect-height))

;;; rect.scm ends here.
