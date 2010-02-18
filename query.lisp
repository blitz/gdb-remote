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

(defgeneric gdb-monitor (server monitor-command)
  (:documentation "Handle a monitor command from GDB.")
  (:method ((server gdb-server) monitor-command)
    ""))

(defgeneric gdb-describe-target (target))
(defgeneric gdb-describe-target-memory-map (target))
(defgeneric gdb-describe-target-spu (target))

(defgeneric gdb-xferrable-read (server xferrable &key &allow-other-keys)
  (:method ((server gdb-server) (x (eql :features)) &key &allow-other-keys)
    (gdb-describe-target server))
  (:method ((server gdb-server) (x (eql :memory-map)) &key &allow-other-keys)
    (gdb-describe-target-memory-map server))
  (:method ((server gdb-server) (x (eql :spu)) &key &allow-other-keys)
    (gdb-describe-target-spu server)))

(defgeneric gdb-xferrable-write (server xferrable vector poffset length &key &allow-other-keys))

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
       (let* ((string (handler-case
                          (octets-to-string (from-hex-string monitor-hex-command))
                        (t ()
                          (return-from gdb-query
                            (format nil "EDecodingError: Try ASCII")))))
              (response (gdb-monitor server string)))
         (to-hex-string (string-to-octets 
                         (if (stringp response)
                             response
                             (format nil "Internal error.~%"))))))
      (("Supported.*")            ; XXX we ignore GDB features
       ;; XXX Use an extra method for this.
       "QStartNoAckMode+;PacketSize=4000;qXfer:features:read+;qXfer:memory-map:read+;qXfer:spu:read+"
       )
      (("Symbol::")
       "OK")
      (("Xfer:(.*):(.*):(.*):(.*),(.*)" pxferrable pdirection annex poffset length)
       (handle-xferrable server (make-keyword (string-upcase pxferrable)) (make-keyword (string-upcase pdirection))
                         (parse-integer poffset :radix #x10) (parse-integer length :radix #x10) annex))
      (("Xfer:(.*):(.*):(.*),(.*)"      pxferrable pdirection poffset length)
       (handle-xferrable server (make-keyword (string-upcase pxferrable)) (make-keyword (string-upcase pdirection))
                         (parse-integer poffset :radix #x10) (parse-integer length :radix #x10)))
      (t
       ""))))

;;; EOF
