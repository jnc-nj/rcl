;; Copyright (c) 2006-2015 Carlos Ungil

(in-package :rcl)

(defclass r-pointer ()
  ((pointer :initarg :pointer :accessor pointer)))

;;  clisp    ffi:foreign-address 
;;  cmu      sys:system-area-pointer
;;  openmcl  ccl:macptr
;;  sbcl     sb-sys:system-area-pointer
;;  allegro  integer
;;  ecl      foreign

(defun r-obj-p (thing)
  (typep thing 'r-pointer))

(defgeneric r-header (sexp))

(defgeneric r-type (sexp))

(defmethod r-header ((sexp r-pointer))
  (r-header (pointer sexp)))

(defmethod r-type ((sexp r-pointer))
  (r-type (pointer sexp)))

(defgeneric r-to-lisp (sexp))

(defgeneric r-obj-describe (sexp))

(defmethod r-to-lisp ((sexp r-pointer))
  (r-to-lisp (pointer sexp)))

(defmethod r-obj-describe ((sexp r-pointer))
  (r-obj-describe (pointer sexp)))

(defmethod print-object ((r-pointer r-pointer) stream)
  (print-unreadable-object (r-pointer stream :type t :identity t)
    (format stream "~s ~s" (r-obj-describe r-pointer) (pointer r-pointer))))

(defvar *unprotect-runner* (make-instance 'simple-tasks:queued-runner))

(defun start-unprotect-runner ()
  (simple-tasks:make-runner-thread *unprotect-runner*)
  (loop until (eq :running (simple-tasks:status *unprotect-runner*)) do (bt:thread-yield)))

(defun schedule-unprotect-task (ptr)
  (simple-tasks:schedule-task
   (make-instance 'simple-tasks:call-task
		  :func (lambda () (rf-unprotect-ptr ptr)))
   *unprotect-runner*))

#+(or abcl allegro cmucl ecl)
(defun schedule-unprotect-task (ptr)
  (rf-unprotect-ptr ptr))

(defun make-r-pointer (ptr)
  (let ((r-pointer (make-instance 'r-pointer :pointer ptr)))
    (rf-protect ptr)
    (trivial-garbage:finalize r-pointer
			      #-bordeaux-threads
			      (lambda () (rf-unprotect-ptr ptr))
			      #+bordeaux-threads
			      (lambda () (schedule-unprotect-task ptr)))
    r-pointer))

