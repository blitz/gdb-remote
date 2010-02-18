;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.gdb-remote)

(defclass* target ()
  ())

(defgeneric register-set-from-vector (target vector)
  (:documentation "Parse a byte vector into a register set
  structure."))

(defgeneric register-set-to-vector (target register-set)
  (:documentation "Transform a register-set into a byte vector."))

(defgeneric gdb-target-registers-as-vector (target)
  (:documentation "Return values of TARGET's registers as byte vector,
in format and order expected by GDB.")
  (:method ((o target))
    "Supposed to be a fallback method in the absence of a more 
efficient pass-through."
    (register-set-to-vector o (gdb-read-registers o))))

(defgeneric gdb-set-target-registers-from-vector (target vector)
  (:documentation "Write TARGET's registers from byte VECTOR,
in format and order expected from GDB.")
  (:method ((o target) vector)
    "Supposed to be a fallback method in the absence of a more
efficient pass-through."
    (gdb-write-registers
     o (register-set-from-vector o vector))))

;;; EOF
