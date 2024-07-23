;;; connection.scm -- Proxy connection.

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

;; This module describes a proxy connection which is used by WebDriver proxy.


;;; Code:

(define-module (web proxy connection)
  #:use-module (oop goops)
  #:use-module (web proxy common)
  #:export (<proxy-connection>
            proxy-connection?
            proxy-connection-host
            proxy-connection-port
            proxy-connection-client
            proxy-connection-client-port
            proxy-connection-target-port
            proxy-connection-tls-session
            proxy-connection-tls-session-set!))

  
(define-class <proxy-connection> ()
  ;; The name of a host that the client wants to connect though the proxy.
  ;;
  ;; <string>
  (host
   #:init-value   #f
   #:init-keyword #:host
   #:getter       proxy-connection-host)

  ;; The port number the client wants to connect to.
  ;;
  ;; <number>
  (port
   #:init-value   #f
   #:init-keyword #:port
   #:getter       proxy-connection-port)

  ;; Proxy client.  The car of the pair is the client socket.
  ;;
  ;; <pair>
  (client
   #:init-value   #f
   #:init-keyword #:client
   #:getter       proxy-connection-client)

  ;; A Scheme port (TCP socket) that is connect to the server.
  ;;
  ;; <port>
  (target-port
   #:init-value   #f
   #:init-keyword #:target-port
   #:getter       proxy-connection-target-port)

  (tls-session
   #:init-value   #f
   #:init-keyword #:tcp-session
   #:getter       proxy-connection-tls-session
   #:setter       proxy-connection-tls-session-set!))



(define-method (%display (connection <proxy-connection>) (port <port>))
  (format port
          "#<proxy-connection ~a:~a ~a>"
          (proxy-connection-host connection)
          (proxy-connection-port connection)
          (object-address/hex-string connection)))

(define-method (display (connection <proxy-connection>) (port <port>))
  (%display connection port))

(define-method (write (connection <proxy-connection>) (port <port>))
  (%display connection port))



(define-method (proxy-connection? x)
  "Check if X is a <proxy-connection> instance."
  (is-a? x <proxy-connection>))



(define-method (proxy-connection-client-port (connection <proxy-connection>))
  "Get the client socket for a proxy CONNECTION."
  (car (proxy-connection-client connection)))

;;; connection.scm ends here.
