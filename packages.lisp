;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(defpackage :blitz.debug.gdb-remote
  (:use :common-lisp :defclass-star :usocket
        :binary-types)
  (:import-from "CL-UTILITIES" "WITH-UNIQUE-NAMES")
  (:import-from "CL-PPCRE" "SCAN-TO-STRINGS")
  (:export "GDB-PROTOCOL-ERROR"
           ;; GDB Server
           "GDB-SERVER"
           "STREAM-OF"
           "ACCEPT-GDB-CONNECTION"
           ;; Targets
           "TARGET-X86"
           "MAKE-X86-REGISTER-SET"
           ;; Server methods
           "RUNNING?"
           "CHECK-INTERRUPT"
           "GDB-INTERRUPT"
           "GDB-KILL"
           "GDB-DETACH"
           "GDB-READ-REGISTERS"
           "GDB-WRITE-REGISTERS"
           "GDB-SET-THREAD"
           "GDB-READ-MEMORY"
           "GDB-WRITE-MEMORY"
           "GDB-CONTINUE-AT"
           "GDB-SINGLE-STEP-AT"
           "GDB-WHY-STOP"
           "GDB-QUERY"
           "GDB-INSERT-BREAKPOINT"
           "GDB-REMOVE-BREAKPOINT"
           "GDB-MONITOR"
           ;; Utilities
           "TO-HEX-STRING"
           "FROM-HEX-STRING"
           "STRING-TO-OCTETS"
           "OCTETS-TO-STRING"
           "REGEX-CASE"
           ;; Constants
           "+REASON-TRAP+"
           "+REASON-INTERRUPT+"
           ;; Client (this is a very bare interface)
           "DO-GDB-CONNECTION"
           "SEND-RAW-COMMAND"
           "GET-RESULT"
           ))

(in-package :blitz.debug.gdb-remote)

(defconstant +reason-trap+ 5)
(defconstant +reason-interrupt+ 2)


;;; EOF
