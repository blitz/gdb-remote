;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.gdb-remote)

(defun to-hex-string (data)
  "Parses data (a vector of unsigned 8-bit integers) and returns its
representation as string of hexadecimal digits."
  (loop 
     with result = (make-string (* 2 (length data)) :element-type 'base-char)
     for in across data
     for out-pos upfrom 0 by 2
     ;; We have to downcase the string, otherwise the first character
     ;; in a reply might be E which GDB interprets as error.
     do (setf (char result out-pos) (char-downcase (digit-char (ldb (byte 4 4) in) 16))
              (char result (1+ out-pos)) (char-downcase (digit-char (ldb (byte 4 0) in) 16)))
     finally (return result)))

(defun from-hex-string (string)
  "Parse a string as produced by TO-HEX-STRING into a vector of 8-bit
integers."
  (assert (and (evenp (length string))
               (every (lambda (c)
                        (digit-char-p c 16))
                      string)))
  (loop 
     with result = (make-array (truncate (length string) 2)
                               :element-type '(unsigned-byte 8))
     for in-pos upfrom 0 below (length string) by 2
     for out-pos upfrom 0
     for hi-digit = (char string in-pos)
     for lo-digit = (char string (1+ in-pos))
     do (setf (aref result out-pos)
              (logior (ash (digit-char-p hi-digit 16) 4)
                      (digit-char-p lo-digit 16)))
     finally (return result)))

(defun octets-to-string (octets)
  #+ sbcl (sb-ext:octets-to-string octets :external-format :ascii)
  #- (or sbcl) (error "Not implemented."))

(defun string-to-octets (string)
  #+ sbcl (sb-ext:string-to-octets string :external-format :ascii)
  #- (or sbcl) (error "Not implemented."))

(defun parse-hex-integer (str)
  ;; XXX Catch parse-error and throw gdb-protocol-error?
  (parse-integer str :radix 16))

;;; EOF
