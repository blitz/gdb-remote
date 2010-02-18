;;; -*- Mode: Lisp -*-
;;;
;;; Copyright (C) 2008, Julian Stecklina
;;;
;;;   ((
;;;    ))     This file is COFFEEWARE. As long as you retain this notice
;;;  |   |o)  you can do whatever you want with this code. If you think,
;;;  |___|jgs it's worth it, you may buy the author a coffee in return.

(in-package :blitz.debug.gdb-remote)

;;;
;;; Globals
;;;
(defvar *trace-exchange* nil
  "Either NIL, which means no tracing, or a positive integer, which is interpreted
as the number of bytes, per packet, to print, for packets in either direction. ")

(defvar *command-hash* (make-hash-table)
  "Maps first characters of incoming packets to reactions -- two-argument functions,
which are passed the server and the command, as a string.")

;;;
;;; Classes
;;;
(defclass* gdb-server ()
  (stream
   (register-set-bytes :initarg :register-set-bytes)
   (no-ack-mode :initform nil)))

(defclass* gdb-extended-server (gdb-server)
  ())

;;;
;;; Conditions
;;;
(defcondition* gdb-protocol-error (error)
  (message 
   errno))

;;;
(defgeneric extended-mode-reaction (server)
  (:method ((o gdb-server)) "")
  (:method ((o gdb-extended-server)) "OK"))

(defgeneric accept-gdb-connection (server port &optional listen-address trace-exchange)
  (:documentation "Wait for a single connection to `server' on `port'")
  (:method ((server gdb-server) port &optional (listen-address "127.0.0.1") (trace-exchange *trace-exchange*))
    (let ((server-socket (socket-listen listen-address port :reuse-address t
                                      :backlog 1))
          (*trace-exchange* trace-exchange))
    (unwind-protect
         (let* ((client (socket-accept server-socket))
                (stream (socket-stream client)))
           (handler-case (unwind-protect 
                              (progn
                                (setf (stream-of server) stream)
                                (format *trace-output* "Connection with ~A established.~%" (get-peer-address client))
                                (handle-protocol server))
                           (close stream))
             ;; XXX: SBCL-ism
             (sb-int:simple-stream-error (c)
               ;; Deal with -EPIPE
               (format *error-output* "~@<; ~@;ERROR: ~A~:@>~%" c))))
         (socket-close server-socket)))))

(defun empty-adjustable-vector ()
  (make-array 0 
              :element-type 'base-char 
              :adjustable t
              :fill-pointer t))

(defun update-checksum (str-or-char &optional (previous-state 0))
  "Calculate checksum for the given string (or update previous-state
for a given character)."
  (loop 
     with state of-type (unsigned-byte 8) = (logand #xFF previous-state)
     for char across (if (stringp str-or-char)
                         str-or-char
                         (string str-or-char))
     do (setf state (logand #xFF (+ state (char-code char))))
     finally (return state)))

(defgeneric gdb-interrupt (server)
  (:documentation "GDB sent something while we ran. We need to interrupt the target.")
  (:method ((server gdb-server))
    ;; Do nothing...
    ))

(defgeneric running? (server)
  (:documentation "Returns true, if the target is running."))

(defgeneric check-interrupt (server)
  (:documentation "Check if GDB wants to interrupt the target.")
  (:method ((server gdb-server))
    (listen (stream-of server))))

(defun block-write-command-p (char)
  "See if we can efficiently read data for this command."
  (member char '(#\M #\G #\X)))

(defgeneric handle-block-write-command (server char)
  (:documentation
   "And efficient data entry path, not reading a data character at a time,
and not running a regex matcher on it either."))

(defun protocol-state-update (stream char state our-checksum their-checksum command
                              packet-fn block-write-fn no-ack-mode &optional verbose)
  (flet ((done (state &key 
                      (our-checksum our-checksum) (their-checksum their-checksum) 
                      (command command))
           (return-from protocol-state-update
             (values state our-checksum their-checksum command))))
    #+ ignore (format t "~&Got ~A. State: ~10A Command: ~A~%" char state command)
    (ecase state
      (:start
       (cond
         ((char/= char #\$)
          ;; Why do we receive another +?
          ;(format *trace-output* "~&Junk character ~C. Ignoring.~%" char)
          (done :start))
         (t
          (done :in-msg1
                :command (empty-adjustable-vector)
                :our-checksum 0))))
      (:in-msg1
       (cond ((and no-ack-mode (block-write-command-p char))
              (funcall block-write-fn char)
              (done :start))
             (t
              (done :in-msg2
                    :command (progn 
                               (vector-push-extend char command)
                               command)
                    :our-checksum (update-checksum char our-checksum)))))
      (:in-msg2
       (cond ((char= char #\#)
              (done :check1))
             (t
              (done :in-msg2
                    :command (progn 
                               (vector-push-extend char command)
                               command)
                    :our-checksum (update-checksum char our-checksum)))))
      (:check1
       (if no-ack-mode
           ;; ACKs are not required
           (done :check2 :their-checksum 0)
           ;; ACKs are required
           (let ((digit (digit-char-p char 16)))
             (assert digit)
             (done :check2 :their-checksum (ash digit 4)))))
      (:check2
       (if no-ack-mode
           ;; ACKs are not required
           (progn
             (funcall packet-fn command)
             (done :start))
           ;; ACKs are required
           (let* ((digit (digit-char-p char 16))
                  (their-checksum (logior their-checksum digit)))
             (cond
               ((= their-checksum our-checksum)
                (write-char #\+ stream)
                (force-output stream)
                (when verbose
                  #+ ignore (format *trace-output* "~&<< ~A~%" command))
                ;; We received a complete request with valid
                (funcall packet-fn command))
               (t
                (write-char #\- stream)
                (force-output stream)
                (when verbose
                  (format *trace-output* "~&Received bad command: ~S" command))))
             (done :start)))))))

(defgeneric handle-protocol (server)
  (:documentation "Handles the low-level details of the protocol and
  maps request from the GDB client to method calls on the server
  object.")
  (:method ((server gdb-server))
    "A nice state-machine."
    (loop 
       with stream = (stream-of server)
       with our-checksum = 0
       with their-checksum = 0
       with command = nil
       with state = :start
       for char = (read-char stream nil nil)
       while char
       ;; If we receive a character while we are running, interrupt
       when (running? server)
       do (gdb-interrupt server)
       do (multiple-value-setq (state our-checksum their-checksum command) 
            (protocol-state-update stream char state our-checksum their-checksum
                                   command
                                   (lambda (command)
                                     ;; Close connection when someone
                                     ;; throws gdb-detach.
                                     (when *trace-exchange*
                                       (format *trace-output* "~&GDB> ~A~:[~;...continued~]~%"
                                               (subseq command 0 (min *trace-exchange* (length command)))
                                               (> (length command) *trace-exchange*))
                                       (force-output *trace-output*))
                                     (when (eq 'gdb-detach 
                                               (catch 'gdb-detach
                                                 (handle-raw-command server command)
                                                 nil))
                                       (return-from handle-protocol)))
                                   (lambda (char)
                                     (handle-block-write-command server char))
                                   (no-ack-mode-of server)
                                   ;; verbose
                                   t)))))

(defgeneric lookup-handler (server command)
  (:method ((server gdb-server) command)
    (if (zerop (length command))
        nil
        (values (gethash (char command 0) *command-hash*)))))

(defun write-response (stream response &optional (checksum (update-checksum response)))
  (write-char #\$ stream)
  (write-sequence response stream)
  (format stream "#~2,'0X" checksum)
  (force-output stream))

(defgeneric handle-raw-command (server command)
  (:documentation "Receives a raw command string as argument, parses
  it and dispatches it to the corresponding method.")
  (:method ((server gdb-server) command)
    (let* ((result (let ((handler (lookup-handler server command)))
                     (if handler
                         ;; TODO Handle gdb-protocol-error
                         (funcall handler server command)
                         "")))
           (checksum (update-checksum result)))
      (when *trace-exchange*
        (format *trace-output* "<reply ~A~:[~;...continued~]~%"
                (subseq result 0 (min *trace-exchange* (length result)))
                (> (length result) *trace-exchange*))
        (force-output *trace-output*))
      (loop
         (write-response (stream-of server) result checksum)
         (when (or (no-ack-mode-of server)
                   (char= #\+ (read-char (stream-of server))))
           (return-from handle-raw-command))))))

(defmacro define-gdb-command (method-name method-args doc (command-char &optional (regex "") (vars method-args)
                                                                        &key (default-method-p t))
                              &body body)
  "Defines a new element of the GDB protocol. The server is bound to
SERVER. Otherwise see the examples."
  (cl-utilities:with-unique-names (command match? submatches)
    `(progn (defgeneric ,method-name (server ,@method-args)
              (:documentation ,doc)
              ,@(when default-method-p
                      `((:method ((server gdb-server) ,@method-args)
                          "Return an empty string."
                          (declare (ignore ,@method-args))
                          ""))))
            (setf (gethash ,command-char *command-hash*)
                  (lambda (server ,command)
                    (multiple-value-bind (,match? ,submatches)
                        (scan-to-strings ,(format nil "^~A$" regex) ,command
                                         ;; Ignore command character
                                         :start 1)
                      (declare (ignorable ,submatches))
                      (if (not ,match?)
                          (progn (cerror "Ignore" "Argument parsing error") "")
                          (let ,(loop 
                                   for submatch upfrom 0
                                   for var-name in vars
                                   if var-name
                                   collect `(,var-name (aref ,submatches ,submatch)))
                            ,(if body
                                 `(progn ,@body)
                                 `(,method-name server ,@method-args))))))))))


;;; EOF
