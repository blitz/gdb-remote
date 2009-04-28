;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.gdb-remote)

(defmacro regex-when ((regex string &rest submatches) &body body)
  (with-unique-names (match? submatch-vector)
    `(multiple-value-bind (,match? ,submatch-vector)
         (scan-to-strings ,regex ,string)
       (declare (ignorable ,submatch-vector))
       (when ,match?
         (let ,(loop 
                  for index upfrom 0
                  for var-name in submatches
                  collect `(,var-name (aref ,submatch-vector ,index)))
         ,@body)))))

(defmacro regex-case (string &body clauses)
  (with-unique-names (the-string block-name)
    `(let ((,the-string ,string))
       (block ,block-name
         ,@(loop 
              for (condition . body) in clauses
              collect (if (eq condition 't) 
                          `(return-from ,block-name
                             (progn ,@body))
                          (destructuring-bind (regex &rest vars)
                              condition
                            `(regex-when (,regex ,the-string ,@vars)
                               (return-from ,block-name
                                 (progn ,@body))))))))))

;;; EOF
