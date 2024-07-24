;; guix.scm --- GNU Guix package recipe    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; Author: Artyom V. Poptsov <poptsov.artyom@gmail.com>
;; Created: 12 July 2024
;;
;; This file is part of Guile-Web-Driver-NG.
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
;;
;; GNU Guix development package. To use as the basis for a development
;; environment, run:
;;
;;  guix environment --pure --container -l guix.scm
;;
;; In the new shell, run:
;;
;;  autoreconf -vif && ./configure && make check
;;
;;; Code:


(use-modules (guix gexp)
             (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (gnu packages texinfo)
             (gnu packages tls)
             (gnu packages man)

             ;; For Guile-SMC
             (gnu packages bash)
             (gnu packages admin)
             (gnu packages base))


(define %source-dir (dirname (current-filename)))


(define guile-web-driver-ng
  (package
   (name "guile-web-driver-ng")
   (version "git")
   (source (local-file %source-dir
                       #:recursive? #t
                       #:select? (git-predicate %source-dir)))
   (build-system gnu-build-system)
   (native-inputs
    (list autoconf
          automake
          pkg-config
          texinfo
          ;; needed when cross-compiling.
          guile-gnutls
          guile-3.0))
   (inputs (list guile-3.0))
   (propagated-inputs (list guile-json-4))
   (home-page "https://github.com/artyom-poptsov/guile-web-driver-ng")
   (synopsis "Web driver (Selenium) client for Guile")
   (description
    "This is a web-driver, or Selenium 2, client.  It's purpose is to automate
browsers, specifically for automatic web server testing.  Chrome or Firefox
can be used as the automated browsers, or it can connect to arbitrary server
providing webdriver interface.  The client implements most of the webdriver
@url{https://www.w3.org/TR/webdriver2/, specification}.")
   (license gpl3+)))

guile-web-driver-ng

;;; guix.scm ends here.
