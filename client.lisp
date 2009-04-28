;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.gdb-remote)

(defclass* gdb-client ()
  (stream
   (no-ack-mode :initform nil)))

(defun do-gdb-connection (host port)
  (make-instance 'gdb-client
                 :stream (socket-stream (socket-connect host port))))

(defun do-gdb-serial-connection (dev)
  (make-instance 'gdb-client
                 :stream (open dev :direction :io :if-exists :append :if-does-not-exist :error)))

(defgeneric enter-no-ack-mode (client)
  (:documentation "")
  (:method ((client gdb-client))
    (send-raw-command client "QStartNoAckMode")
    (setf (no-ack-mode-of client) t)
    (assert (string= "OK" (get-result client)))))

(defgeneric send-raw-command (client command)
  (:documentation "")
  (:method ((client gdb-client) command)
    (let* ((checksum (update-checksum command))
           (packet (format nil "$~A#~2,'0X" command checksum)))
      (loop
           (write-string packet (stream-of client))
         (force-output (stream-of client))
         (when (or (no-ack-mode-of client)
                   (char= (read-char (stream-of client)) #\+))
           (return-from send-raw-command))))))

(defgeneric get-result (client &optional wait)
  (:method ((client gdb-client) &optional (wait t))
    (when (or wait
              (listen (stream-of client)))
      (loop with stream = (stream-of client)
         with our-checksum = 0
         with their-checksum = 0
         with command = nil
         with state = :start
         for char = (read-char stream nil nil)
         while char
         do (multiple-value-setq (state our-checksum their-checksum command) 
              (protocol-state-update stream char state our-checksum their-checksum
                                     command
                                     (lambda (result)
                                       (return-from get-result
                                         result))
                                     (no-ack-mode-of client)))))))

;;; Benchmark low-level I/O

#+ ignore
(progn
  (defun do-where (c)
    (send-raw-command c "?")
    (get-result c))
  
  (defun do-registers (c)
    (send-raw-command c "g")
    (get-result c))

  (defun do-stepi (c)
    (send-raw-command c "s")
    (get-result c))

  (defun do-memory-read-n (c start size block)
    (loop 
       for cur upfrom start by block
       repeat (ceiling size block)
       do (progn
            (send-raw-command c (string-downcase (format nil "m~x,~x" cur block)))
            (assert (= (length (get-result c)) (* 2 block))))))

  (defun time-fn (name fn count)
    (let ((units internal-time-units-per-second)
          (start (get-internal-real-time)))
      (loop repeat count
         do (funcall fn))
      (let ((end (get-internal-real-time)))
        (format t "~&~A (~A repeats): ~8,3F ms~%"
                name count
                (* 1000
                   (/ (- end start) 
                      units
                      count)))
        (force-output))))
  
  (defun clear-cache (c)
    (return-from clear-cache t)
    (send-raw-command c (format nil "qRcmd,~A"
                                (to-hex-string (sb-ext:string-to-octets "clear-cache"))))
    (assert (string= "4f4b" (get-result c)))
    (sb-ext:gc :full t)
    ;; Got OK?
    )

  (defun do-bench (c)
    (get-result c nil)
    (send-raw-command c "Hc-1")
    (format t "~A~%" (get-result c))
    (force-output)
    (time-fn "where (?)" (lambda ()
                           (do-where c))
             2000)
    (time-fn "registers (g)" (lambda ()
                               (do-registers c))
             2000)
    (clear-cache c)
    (time-fn "memory read 32K/64" (lambda ()
                                     (do-memory-read-n c #xc0000000 #x8000 64))
             10)
    (clear-cache c)
    (time-fn "memory read 32K/128" (lambda ()
                                     (do-memory-read-n c #xc0000000 #x8000 128))
             10)
    (clear-cache c)
    (time-fn "memory read 32K/512" (lambda ()
                                     (do-memory-read-n c #xc0000000 #x8000 512))
             10)
    (clear-cache c)
    (time-fn "memory read 32K/1024" (lambda ()
                                      (do-memory-read-n c #xc0000000 #x8000 1024))
             10)
    (clear-cache c)
    (time-fn "memory read 32K/2048" (lambda ()
                                      (do-memory-read-n c #xc0000000 #x8000 2048))
             10)
    )

  (defvar *c*)
  #+ ignore
  (setq *c* (do-gdb-serial-connection "/dev/ttypl"))
  )

;;; Linux GDB 9600N8
;; where (?) (200 repeats):   16.700 ms
;; registers (g) (200 repeats):  160.150 ms
;; stepi (100 repeats):   45.790 ms
;; memory read 32K/64 (2 repeats): 88773.000 ms
;; memory read 32K/128 (2 repeats): 81908.000 ms
;; memory read 32K/512 (2 repeats): 76763.500 ms
;; memory read 32K/1024 (2 repeats): 75840.500 ms
;; memory read 32K/2048 (2 repeats): 75409.500 ms

;;; Linux GDB 115200N8
;; where (?) (200 repeats):    5.055 ms
;; registers (g) (200 repeats):   19.900 ms
;; stepi (100 repeats):    7.300 ms
;; memory read 32K/64 (2 repeats): 10251.500 ms
;; memory read 32K/128 (2 repeats): 8528.500 ms
;; memory read 32K/512 (2 repeats): 7246.500 ms
;; memory read 32K/1024 (2 repeats): 7038.000 ms
;; memory read 32K/2048 (2 repeats): 6883.000 ms

;;; Nova Monitor FW (warm cache)
;;; XXXXX BOGUS -> REPEAT! XXXXX
;; where (?) (200 repeats):   79.990 ms
;; registers (g) (200 repeats):   80.015 ms
;; memory read 32K/64 (2 repeats): 40963.500 ms
;; memory read 32K/128 (2 repeats): 20481.500 ms
;; memory read 32K/512 (2 repeats): 5125.500 ms
;; memory read 32K/1024 (2 repeats): 2560.000 ms
;; memory read 32K/2048 (2 repeats): 1287.500 ms

;; where (?) (2000 repeats):    0.289 ms
;; registers (g) (2000 repeats):    0.324 ms
;; stepi (100 repeats):    4.590 ms
;; memory read 32K/64 (10 repeats):  118.300 ms
;; memory read 32K/128 (10 repeats):  108.500 ms
;; memory read 32K/512 (10 repeats):   59.600 ms
;; memory read 32K/1024 (10 repeats):   43.000 ms
;; memory read 32K/2048 (10 repeats):  641.100 ms

;;; Cold caches
;;; XXXXX BOGUS -> REPEAT! XXXXX
;; memory read 32K/64 (2 repeats): 40967.000 ms
;; memory read 32K/128 (1 repeats): 20490.000 ms
;; memory read 32K/512 (1 repeats): 5120.000 ms
;; memory read 32K/1024 (1 repeats): 2561.000 ms
;; memory read 32K/2048 (1 repeats): 1284.000 ms

;; memory read 32K/64 (10 repeats):  145.900 ms
;; memory read 32K/128 (10 repeats):  106.500 ms
;; memory read 32K/512 (10 repeats):   60.800 ms
;; memory read 32K/1024 (10 repeats):   54.500 ms
;; memory read 32K/2048 (10 repeats):  643.100 ms

;;; EOF
