;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.gdb-remote)

(define-gdb-command gdb-set-query (query-string)
     "We are asked to do something."
     (#\Q "(.*)" (query-string) :default-method-p nil))

(defmethod gdb-set-query ((server gdb-server) query-string)
  (cond
    ((string= "StartNoAckMode" query-string)
     (setf (no-ack-mode-of server) t)
     "OK")
    (t "")))

(define-gdb-command gdb-query (query-string)
    "We received an extended query packet."
    (#\q "(.*)" (query-string) :default-method-p nil))

(defgeneric gdb-monitor (server monitor-command rest-arg)
  (:documentation "Handle a monitor command from GDB.")
  (:method ((server gdb-server) monitor-command rest-arg)
    ""))

(defgeneric gdb-describe-target (target))
(defgeneric gdb-describe-target-memory-map (target))
(defgeneric gdb-describe-target-spu (target))

(defgeneric gdb-xferrable-write (server xferrable vector poffset length &key &allow-other-keys))
(defgeneric gdb-xferrable-read (server xferrable &key &allow-other-keys)
  (:method ((server gdb-server) (x (eql :features)) &key &allow-other-keys)
    (gdb-describe-target server))
  (:method ((server gdb-server) (x (eql :memory-map)) &key &allow-other-keys)
    (gdb-describe-target-memory-map server))
  (:method ((server gdb-server) (x (eql :spu)) &key &allow-other-keys)
    (gdb-describe-target-spu server)))

(defgeneric gdb-handle-query (server query arguments)
  (:method ((o gdb-server) query args) "")
  (:method ((o gdb-server) (q (eql :supported)) args)
    "QStartNoAckMode+;PacketSize=4000"))

(let (xferrable data)

  (defun handle-xferrable (server pxferrable pdirection poffset length &optional annex)
    (ecase pdirection
      (:read
       (unless (eq pxferrable xferrable)
         (setf xferrable xferrable
               data (gdb-xferrable-read server pxferrable :annex annex)))
       (if (= poffset (length data))
           "l"
           (concatenate 'string "m" (subseq data poffset (min (length data) (+ poffset length))))))
      (:write
       (error "Xfer writes not supported yet."))))

  (defmethod gdb-query ((server gdb-server) query-string)
    (regex-case query-string
      (("Rcmd,(.*)" monitor-hex-command)
       (let* ((command (handler-case
                           ;; Interpret only the first piece -- pass the rest to the handler as-is
                           (let ((decoded-string (octets-to-string (from-hex-string monitor-hex-command))))
                             (multiple-value-bind (cmd rest)
                                 (split-sequence:split-sequence #\Space
                                                                decoded-string
                                                                :count 1
                                                                :remove-empty-subseqs t)
                               (cons (make-keyword (string-upcase (first cmd)))
                                     (subseq decoded-string rest))))
                         (t ()
                           (return-from gdb-query
                             (format nil "EDecodingError: Try ASCII")))))
              (response-values (or (multiple-value-list (gdb-monitor server (car command) (cdr command)))
                                   '("; No values"))))
         (to-hex-string (string-to-octets (apply #'concatenate 'string
                                                 (loop :for (r . rest) :on response-values
                                                    :collect r
                                                    :collect #(#\Newline)))))))
      (("Xfer:(.*):(.*):(.*):(.*),(.*)" pxferrable pdirection annex poffset length)
       (handle-xferrable server (make-keyword (string-upcase pxferrable)) (make-keyword (string-upcase pdirection))
                         (parse-integer poffset :radix #x10) (parse-integer length :radix #x10) annex))
      (("Xfer:(.*):(.*):(.*),(.*)"      pxferrable pdirection poffset length)
       (handle-xferrable server (make-keyword (string-upcase pxferrable)) (make-keyword (string-upcase pdirection))
                         (parse-integer poffset :radix #x10) (parse-integer length :radix #x10)))
      (t
       (let ((sep-posn (position-if (rcurry #'member '(#\, #\: #\; #\?)) query-string)))
         (gdb-handle-query server (make-keyword (string-upcase (subseq query-string 0 sep-posn)))
                           (when (and sep-posn (/= (1+ sep-posn) (length query-string)))
                             (subseq query-string (1+ sep-posn)))))))))

;;; EOF
