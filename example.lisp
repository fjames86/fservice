;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(defpackage #:fservice.example
  (:use #:cl #:fservice))

(in-package #:fservice.example)

;; ----------------------------------
;; example 

(defvar *service-name* "lisp-service")
(defvar *log* nil)
(defun service-log (format &rest args)
  (pounds.log:write-message *log* :info (apply #'format nil format args)))

(defvar *service-handle* nil)
(defvar *lock* nil)
(defvar *condv* nil)

(defun control-handler (control)
  (service-log "LISP-CONTROL-HANDLER ~A" control)
  (case control
    ((:shutdown :stop)
     (service-log "Received stop message")
     ;; set pending
     (set-service-status *service-handle* :stop-pending)
     (service-log "Notifying the condition variable")
     (bt:condition-notify *condv*))))

(defun service-main (args)
  (declare (ignore args))
  
  (handler-case 
      (progn
  (service-log "Service main ~A" (bt:current-thread))

  (setf *lock* (bt:make-lock)
        *condv* (bt:make-condition-variable))

  (service-log "Starting service main")

  ;; setup here
  (let ((handle (register-service-control-handler #'control-handler *service-name*)))
    (setf *service-handle* handle)

    (service-log "setting service status to running")
    (set-service-status handle :running)

    (service-log "starting swank")
;;    (swank-loader:init)
    (swank:create-server :port 4004
                         :style swank:*communication-style*
                         :dont-close t)
    (bt:with-lock-held (*lock*)
      (bt:condition-wait *condv* *lock*))

    (swank:stop-server 4004)

    ;; when done set the service status to stopped and we will be killed
    (service-log "stopping")

    (set-service-status handle :stopped)))
    (error (e)
      (service-log "Error: ~A" e)))

    nil)

(defun main ()
  (setf *log* (pounds.log:open-log :path (merge-pathnames "service.log" (user-homedir-pathname))))
  (service-log "Starting up")

  (handler-case (start-service #'service-main *service-name*)
    (error (e) (service-log "start service failed ~A" e)))

  (service-log "~A Main exiting" (bt:thread-name (bt:current-thread))))
