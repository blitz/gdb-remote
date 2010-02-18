;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.gdb-remote)

(define-gdb-command gdb-kill ()
    "Kill program."
    (#\k))

(define-gdb-command gdb-detach () 
    "Prepare server for client detach"
    (#\D))

(defmethod gdb-detach ((server gdb-server))
  "Detach (and do not send a response)"
  (throw 'gdb-detach 'gdb-detach))

(defmethod gdb-kill ((server gdb-server))
  "Same as GDB-DETACH. Must be overridden to implement any real
killing."
  (throw 'gdb-detach 'gdb-detach))

(define-gdb-command gdb-extended-mode ()
    "Enable extended mode."
    (#\!)
  (extended-mode-reaction server))

(define-gdb-command gdb-read-registers ()
    "Read registers"
    (#\g)
  (to-hex-string (gdb-target-registers-as-vector server)))

(define-gdb-command gdb-write-registers (register-set)
    "Write registers. Return value is ignored."
    (#\G "(.*)")
  (gdb-set-target-registers-from-vector server (from-hex-string register-set))
  "OK")

(define-gdb-command gdb-read-register (nr)
    "Read registers"
    (#\p "(.*)")
  (to-hex-string (write-to-string (gdb-read-target-register server nr))))

(define-gdb-command gdb-write-register (nr value)
    "Write registers. Return value is ignored."
    (#\P "(.*)=(.*)")
  (gdb-write-target-register server (parse-integer nr :radix #x10) (parse-integer value :radix #x10))
  "OK")

(define-gdb-command gdb-set-thread (domain thread)
    "Set thread for specific operations"
    (#\H "(.)([+-]?[0-9]+)")
  (gdb-set-thread server domain (parse-integer thread)))

(define-gdb-command gdb-read-memory (addr size)
    "Read a slice of the target's memory"
    (#\m "(.*),(.*)")
  (let ((res (gdb-read-memory server 
                              (parse-hex-integer addr)
                              (parse-hex-integer size))))
    (if (and (stringp res)
             (not (zerop (length res)))
             (char= (char res 0) #\E))
        res                             ; Pass through error
        (to-hex-string res))))

(define-gdb-command gdb-write-memory (addr data)
    "Write to the target's memory"
    (#\M "(.*),(.*):(.*)" (addr size data-string))
  (let ((data-vector (from-hex-string data-string)))
    (if (/= (length data-vector)
            (parse-integer size :radix 16))
        (error 'gdb-protocol-error
               :message "Data sizes contradict"
               :errno 1)
        (gdb-write-memory server
                          (parse-hex-integer addr)
                          data-vector))))

(defun hex-integer-or-nil (str)
  (if (zerop (length str))
      nil
      (parse-hex-integer str)))

(define-gdb-command gdb-continue-at (addr)
    "Continue at the specified address (which might be NIL in which
    case resume where left of)"
    (#\c "(.*)")
  (gdb-continue-at server (hex-integer-or-nil addr)))

(define-gdb-command gdb-single-step-at (addr)
    "Execute a single step at the specified address (which might be
    NIL in which case single step from where left of"
    (#\s "(.*)")
  (gdb-single-step-at server (hex-integer-or-nil addr)))

(define-gdb-command gdb-why-stop ()
    "Why did we stop?"
    (#\?))

(defun break-type-to-symbol (type)
  (ecase (char type 0)
    (#\0 :software)
    (#\1 :hardware)
    (#\2 :write)
    (#\3 :read)
    (#\4 :access)))

(define-gdb-command gdb-insert-breakpoint (type address length)
    "Insert a breakpoint. Type is either :SOFTWARE, :HARDWARE, :WRITE, :READ, :ACCESS."
    (#\Z "(.),(.*),(.*)")
  (gdb-insert-breakpoint server
                         (break-type-to-symbol type)
                         (parse-hex-integer address)
                         (parse-hex-integer length)))


(define-gdb-command gdb-remove-breakpoint (type address length)
    "Insert a breakpoint. Type is either :SOFTWARE, :HARDWARE, :WRITE, :READ, :ACCESS."
    (#\z "(.),(.*),(.*)")
  (gdb-remove-breakpoint server
                         (break-type-to-symbol type)
                         (parse-hex-integer address)
                         (parse-hex-integer length)))

(defgeneric gdb-extended-command (server command arguments)
  (:method ((o gdb-server) command arguments)
    ""))

(define-gdb-command gdb-extended (command)
    "Extended command."
    (#\v "(.*)")
  (let ((sep-posn (position-if (rcurry #'member '(#\? #\: #\;)) command)))
    (gdb-extended-command server (make-keyword (string-upcase (subseq command 0 sep-posn)))
                          (when (and sep-posn (/= (1+ sep-posn) (length command)))
                            (subseq command (1+ sep-posn))))))

;;; EOF
