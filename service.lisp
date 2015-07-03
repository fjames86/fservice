;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(defpackage #:fservice 
  (:use #:cl #:cffi)
  (:export #:start-service
           #:register-service-control-handler
           #:set-service-status))

(in-package #:fservice)

;; ------------------------------------------------------

;; struct _SERVICE_TABLE_ENTRY {
;;   LPTSTR                  lpServiceName;
;;   LPSERVICE_MAIN_FUNCTION lpServiceProc;
;; } SERVICE_TABLE_ENTRY
(defcstruct service-table-entry 
  (name :pointer)
  (fn :pointer))
(defun init-service-table-entry (ptr fn-ptr n-ptr)
  (declare (type cffi:foreign-pointer ptr fn-ptr n-ptr))
  (with-foreign-slots ((name fn) ptr (:struct service-table-entry))
    (setf name n-ptr
          fn fn-ptr))
  ptr)

(defcfun ("StartServiceCtrlDispatcherA" %start-service :convention :stdcall) :boolean
  (table :pointer))

(defvar *ctrl-dispatcher-hook* nil)
(defcallback (%control-dispatcher :convention :stdcall) :void
    ((argc :uint32)
     (argv :pointer))
  (let ((strs (loop :for i :below argc :collect 
                 (foreign-string-to-lisp (mem-aref argv :pointer i))))
        (lock (bt:make-lock))
        (l2 (bt:make-lock))
        (condv (bt:make-condition-variable)))
    
    (bt:acquire-lock lock)
    
    ;; start the Lisp thread, but block its execution until we are ready 
    (bt:make-thread (lambda () 
                      (bt:with-lock-held (lock)
                        (funcall *ctrl-dispatcher-hook* strs)
                        (bt:condition-notify condv)))
                    :name "service-main-thread")
    
    (bt:release-lock lock)
    
    (bt:with-lock-held (l2)
      (bt:condition-wait condv l2)))
  nil)

(defun start-service (fn name)
  "Start the service control dispatcher. 
FN ::= function taking a single argument which is a list of strings for the command line arguments.
NAME ::= string naming the service name.

This function blocks until the service main function has completed.
"
  (declare (type function fn)
           (type string name))
  (with-foreign-object (table '(:struct service-table-entry) 2)
    (with-foreign-string (n name)
      (init-service-table-entry (mem-aptr table '(:struct service-table-entry) 0)
                                (get-callback '%control-dispatcher)
                                n)
      (init-service-table-entry (mem-aptr table '(:struct service-table-entry) 1)
                                (null-pointer)
                                (null-pointer))
      (setf *ctrl-dispatcher-hook* fn)
      (let ((res (%start-service table)))
        (unless res 
          (error "Failed to start service"))))))

;; ----------------------------------

(defun translate-control (id)
  (case id
    (3 :continue)
    (4 :interrogate)
    (2 :pause)
    (5 :shutdown)
    (1 :stop)
    (otherwise id)))

(defvar *service-control-handler-hook* nil)

(defcallback (%control-handler-ex :convention :stdcall) :uint32
    ((control :uint32)
     (event :uint32)
     (edata :pointer)
     (context :pointer))
  (declare (ignore event edata context))
  (funcall *service-control-handler-hook* (translate-control control)))

(defcfun ("RegisterServiceCtrlHandlerExA" %register-control-handler-ex :convention :stdcall) :pointer
  (name :pointer)
  (proc :pointer)
  (context :pointer))

(defun register-service-control-handler (fn name)
  "Register the service control handler to receive control messages.
FN ::= function accepting a single argument which is a symbol naming the control message.
NAME ::= string naming the service name.

Returns the service handle which should be passed to set-service-status."
  (declare (type function fn)
           (type string name))
  (setf *service-control-handler-hook* fn)
  (with-foreign-string (s name)
    (let ((handle (%register-control-handler-ex s (get-callback '%control-handler-ex) (null-pointer))))
      (if (null-pointer-p handle)
          (error "Failed to register handler")
          handle))))

;; -----------------------------

;; struct _SERVICE_STATUS {
;;   DWORD dwServiceType;
;;   DWORD dwCurrentState;
;;   DWORD dwControlsAccepted;
;;   DWORD dwWin32ExitCode;
;;   DWORD dwServiceSpecificExitCode;
;;   DWORD dwCheckPoint;
;;   DWORD dwWaitHint;
;; } SERVICE_STATUS,
(defcstruct service-status 
  (type :uint32)
  (state :uint32)
  (controls :uint32)
  (exit-code :uint32)
  (specific-exit-code :uint32)
  (checkpoint :uint32)
  (hint :uint32))

(defconstant +service-type-win32+ #x30)
(defun init-service-status (ptr status &optional exit)
  (with-foreign-slots ((type state controls exit-code specific-exit-code checkpoint hint) ptr (:struct service-status))
    (let ((code (ecase status
                  (:stopped 1)
                  (:start-pending 2)
                  (:stop-pending 3)
                  (:running 4)
                  (:continue-pending 5)
                  (:pause-pending 6)
                  (:paused 7))))
      (setf type +service-type-win32+
            state code
            controls #x05 ;; accept stop | accept shutdown
            exit-code (or exit 0)
            specific-exit-code 0
            checkpoint 0
            hint 0)))
  ptr)

(defcfun ("SetServiceStatus" %set-service-status :convention :stdcall) :boolean
  (handle :pointer)
  (status :pointer))
  
;; #define SERVICE_STOPPED                        0x00000001
;; #define SERVICE_START_PENDING                  0x00000002
;; #define SERVICE_STOP_PENDING                   0x00000003
;; #define SERVICE_RUNNING                        0x00000004
;; #define SERVICE_CONTINUE_PENDING               0x00000005
;; #define SERVICE_PAUSE_PENDING                  0x00000006
;; #define SERVICE_PAUSED                         0x00000007

;; #define SERVICE_ACCEPT_STOP                    0x00000001
;; #define SERVICE_ACCEPT_PAUSE_CONTINUE          0x00000002
;; #define SERVICE_ACCEPT_SHUTDOWN                0x00000004
(defun set-service-status (handle state &optional exit-code)
  "Set the service status. 
HANDLE ::= a handle as returned from the call to register-service-control-handler.
STATE ::= a symbol naming the service state.
EXIT-CODE ::= an integer to set the exit code to, defaults to 0.
"
  (with-foreign-object (s '(:struct service-status))
    (init-service-status s 
                         state
                         exit-code)
    (let ((res (%set-service-status handle s)))
      (unless res (error "error setting service status")))))



;; --------------

