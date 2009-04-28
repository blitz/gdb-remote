;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.gdb-remote)

(defclass* target-x86 (target)
  ())

(define-unsigned uint32 4)

(macrolet ((generate-register-set (&rest register-names)
             `(progn
                (define-binary-struct (x86-register-set
                                       (:conc-name "REG-")) 
                    ()
                  ,@(loop for reg in register-names
                       collect `(,reg 0 :binary-type uint32)))
                (export ',(mapcar (lambda (s)
                                    (intern (format nil "REG-~A" s) "BLITZ.DEBUG.GDB-REMOTE"))
                                  register-names)))))
  (generate-register-set eax ecx edx ebx esp ebp esi edi eip eflags cs
                         ss ds es fs gs))

(defmethod register-set-from-vector ((target target-x86) vector)
  (let ((*endian* :little-endian))
    (with-binary-input-from-vector (v vector)
      (read-binary 'x86-register-set v))))

(defmethod register-set-to-vector ((target target-x86) register-set)
  (let ((*endian* :little-endian))
    (with-binary-output-to-vector (v)
      (write-binary 'x86-register-set v register-set))))

;;; EOF
