# fservice
Run Lisp image as a Windows service

## 1. Introduction
Windows services comparable to unix daemons, i.e. long-running processes which are independant of user login sessions. However, they are considerably more annoying to setup than the unix equivalent, particularly for Lisp, because
you have to follow a specified API. Unfortunately, the API requires the main control to be transferred via a 
callback from a foreign thread which can cause problems for many Lisp implementations. This library attempts
to mitigate this somewhat by immediately creating a Lisp thread and transfering control to that. The foreign
callback then sleeps until the Lisp thread completes.

## 2. Usage
You need a Lisp implementation which supports building executable images, foreign callbacks and, cruicially, callbacks from foreign threads. This was developed using SBCL 1.2.11 on Windows Server 2012 R2, but it should work with other implementations.

It is assumed the reader is already familiar with writing Windows service, see the relevant documentation 
on MSDN for more information and examples in the C programming language.

Your Lisp entry point must immediately call `START-SERVICE`, passing a callback for your service main function.
This should immediately register a control handler using `REGISTER-SERVICE-CONTROL-HANDLER` 
and then keep doing work until the stop event is signalled. Use `SET-SERVICE-STATUS` to report current service 
status back to SCM.


```
(defun control-handler (control)
  (case control
    ((:stop :shutdown)
     ;; the stop/shutdown message was sent to us
     )))

(defun service-main (args)
  (let ((handle (register-service-control-handler #'control-handler "my-lisp-service")))
    ;; do work here until the STOP event is signalled by the control handler
  ))

(defun main ()
  (start-service #'service-main "my-lisp-service"))
```

## 3. Example
The example is for SBCL, build the example by running:
```
> sbcl --load build.lisp
```
This generates a file service.exe.

Create the service and start it 
```
> sc create lisp-service start= auto binPath=path/to/service.exe DisplayName= "example lisp service"
> sc start lisp-service 
> sc query lisp-service
> sc stop lisp-service 
```

Try connecting your SLIME session to the swank server on port 4004.

## 4. License
Licensed under the terms of the MIT license.

Frank James
July 2015.



