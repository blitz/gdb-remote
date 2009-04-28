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

;;; EOF
