;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2009, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(defpackage :blitz.debug.gdb-remote.system
  (:use :common-lisp :asdf))
(in-package :blitz.debug.gdb-remote.system)

(defsystem :gdb-remote
  :depends-on (:alexandria :defclass-star :usocket :cl-ppcre :cl-utilities
                           :binary-types)
  :serial t
  :components ((:file "packages")
               (:file "utility-macros")
               (:file "utilities")
               (:file "target")
               (:file "target-x86")
               (:file "server")
               (:file "client")
               (:file "commands")
               (:file "query")
               (:file "dummy")
               ))

;;; EOF
