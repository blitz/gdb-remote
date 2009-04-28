;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2009, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.gdb-remote)

(defclass dummy-gdb-server (gdb-server)
  ())

(defmethod running? ((server dummy-gdb-server))
  nil)