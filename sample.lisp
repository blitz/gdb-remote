;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(defpackage :blitz.debug.gdb-remote.sample
  (:use :common-lisp :defclass-star
        :blitz.debug.gdb-remote))
(in-package :blitz.debug.gdb-remote.sample)

(defclass* sample-server (target-x86 gdb-server)
  ((running? :accessor running? :initform t)))

(defun test-sample-server (&optional (port 9000))
  (accept-gdb-connection (make-instance 'sample-server) port))


(defmethod gdb-interrupt ((server sample-server))
  (format *trace-output* "Interrupt!~%")
  (setf (running? server) nil))


(defmethod gdb-set-thread ((server sample-server) domain thread)
  (declare (ignore domain))
  (if (or (= thread -1)
          (= thread 0))
      "OK"
      "E00"))

(defmethod gdb-why-stop ((server sample-server))
  "S00")


(defmethod gdb-read-registers ((server sample-server))
  (make-x86-register-set :eax #xDEADBEEF))

(defmethod gdb-write-registers ((server sample-server) register-set)
  )

(defmethod gdb-continue-at ((server sample-server) addr)
  (setf (running? server) t)
  (loop 
     while (not (check-interrupt server))
     do (sleep 1))
  "S00"
  )

;;; EOF
