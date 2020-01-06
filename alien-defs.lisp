;; Copyright (c) 2006-2015 Carlos Ungil

(in-package :rcl)

(cffi:defctype SEXP :pointer)

;;  :type  5  :obj   1  :named 2  :gp    16  :mark 1
;;  :debug 1  :trace 1  :fin   1  :gcgen  1  :gccls 3
(cffi:defcstruct sxpinfo_struct
  (bitfield :unsigned-int))

(cffi:defcstruct primsxp_struct
  (offset :int))

(cffi:defcstruct symsxp_struct
  (pname :pointer)
  (value :pointer)
  (internal :pointer))

(cffi:defcstruct listsxp_struct
  (carval :pointer)
  (cdrval :pointer)
  (tagval :pointer))

(cffi:defcstruct envsxp_struct
  (frame :pointer)
  (enclos :pointer)
  (hashtab :pointer))

(cffi:defcstruct closxp_struct
  (formals :pointer)
  (body :pointer)
  (env :pointer))

(cffi:defcstruct promsxp_struct
  (value :pointer)
  (expr :pointer)
  (env :pointer))

(cffi:defcunion SEXPREC_UNION
 (primsxp (:struct primsxp_struct))
 (symsxp (:struct symsxp_struct))
 (listsxp (:struct listsxp_struct))
 (envsxp (:struct envsxp_struct))
 (closxp (:struct closxp_struct))
 (promsxp (:struct promsxp_struct)))

(cffi:defcstruct SEXPREC
  (sxpinfo (:struct sxpinfo_struct))
  (attrib :pointer)
  (gengc_next_node :pointer)
  (gengc_prev_node :pointer)
  (u (:union SEXPREC_UNION)))

(cffi:defcstruct vecsxp_struct
  (length :int)
  (truelength :int))

(cffi:defcstruct VECTOR_SEXPREC
  (sxpinfo (:struct sxpinfo_struct))
  (attrib :pointer)
  (gengc_next_node :pointer)
  (gengc_prev_node :pointer)
  (vecsxp (:struct vecsxp_struct)))

(cffi:defcunion SEXPREC_ALIGN
  (s (:struct VECTOR_SEXPREC))
  (align :double))

(cffi:defcvar ("R_GlobalEnv" :read-only t) SEXP)
(cffi:defcvar ("R_UnboundValue" :read-only t) SEXP)
(cffi:defcvar ("R_NilValue" :read-only t) SEXP)
;;; seen in RCLG (also the idea of using :read-only)
(cffi:defcvar ("R_InputHandlers" :read-only t) SEXP)
(cffi:defcvar ("R_NamesSymbol" :read-only t) SEXP)
(cffi:defcvar ("R_DimSymbol" :read-only t) SEXP)

(cffi:defcvar ("R_NaN" :read-only t) :double)
(cffi:defcvar ("R_PosInf" :read-only t) :double)
(cffi:defcvar ("R_NegInf" :read-only t) :double)

(cffi:defcvar ("R_NaReal" :read-only t) :double) ;; real
(cffi:defcvar ("R_NaInt" :read-only t) :int) ;; logical, integer
(cffi:defcvar ("R_NaString" :read-only t) SEXP) ;; string

(defmacro with-float-traps-masked (&rest body)
  #+cmu `(ext:with-float-traps-masked
	    (:divide-by-zero :invalid)
	  ,@body)
  #+sbcl `(sb-int:with-float-traps-masked 
	      (:underflow :overflow :inexact :divide-by-zero :invalid)
	    ,@body)
  #-(or sbcl cmu) `(progn ,@body))

(defmacro in-r-thread (&rest body)
  `(simple-tasks:with-body-as-task (*r-runner*)
     ,@body))

#+(or abcl allegro cmucl ecl)
(defmacro in-r-thread (&rest body)
  `(progn ,@body))

(defvar *r-runner* (make-instance 'simple-tasks:queued-runner))

(defun start-r-runner ()
  (simple-tasks:make-runner-thread *r-runner*)
  (loop until (eq :running (simple-tasks:status *r-runner*)) do (bt:thread-yield)))

(defmacro defcfun (name-and-options return-type &body args)
  "Defines a Lisp function that calls a foreign function."
  (let ((docstring (when (stringp (car args)) (pop args))))
    (multiple-value-bind (lisp-name foreign-name options)
        (cffi::parse-name-and-options name-and-options)
      (let ((name (intern (concatenate 'string "%" (symbol-name lisp-name)))))
	(if (eq (cffi::lastcar args) '&rest)
	    (append
	     (cffi::%defcfun-varargs name foreign-name return-type
				     (butlast args) options docstring)
	     `((defun ,lisp-name (,@(mapcar #'car (butlast args)))
		 (in-r-thread
		  (with-float-traps-masked
		      (,name ,@(mapcar #'car (butlast args))))))))
	    (append	    
	     (cffi::%defcfun name foreign-name return-type 
			     args options docstring)
	     `((defun ,lisp-name (,@(mapcar #'car args))
		 (in-r-thread
		  (with-float-traps-masked
		      (,name ,@(mapcar #'car args))))))))))))

(defcfun "Rf_initEmbeddedR" :int (argc :int) (argv :pointer))
(defcfun "Rf_initialize_R" :int (argc :int) (argv :pointer))
(defcfun "setup_Rmainloop" :void)
(defcfun "run_Rmainloop" :void)
(defcfun "Rf_endEmbeddedR" :void (fatal :int))
(defcfun "Rf_install" SEXP (str :string))
(defcfun "Rf_findFun" SEXP (fun SEXP) (rho SEXP))
(defcfun "Rf_findVar" SEXP (var SEXP) (rho SEXP))
(defcfun "Rf_eval" SEXP (expr SEXP) (rho SEXP))
(defcfun "R_tryEval" SEXP (expr SEXP) (rho SEXP) (error :pointer))

(defcfun "Rf_allocVector" SEXP (type :unsigned-int) (length :int))
(defcfun "Rf_mkChar" SEXP (string :string))
(defcfun "Rf_mkString" SEXP (string :string))

;; SEXP Rf_mkCharLen(const char *, int);
;; http://stat.ethz.ch/R-manual/R-devel/doc/manual/R-exts.html#Character-encoding-issues
;; SEXP Rf_mkCharCE(const char *, cetype_t);
;; SEXP Rf_mkCharLenCE(const char *, int, cetype_t);  //new in 2.8.0 

(defcfun "Rf_protect" SEXP (expr SEXP))
(defcfun "Rf_unprotect" :void (n :int))
(defcfun "Rf_unprotect_ptr" :void (expr SEXP))

(defcfun "R_gc" :void)

#+windows (defcfun "R_WaitEvent" :void)
(defcfun "R_ProcessEvents" :void)

#-windows (defcfun "R_checkActivity" :pointer (usec :int) (ignore-stdin :int))
#-windows (defcfun "R_runHandlers" :void (handler :pointer) (what :pointer))

(defcfun "R_ReplDLLinit" :void)
(defcfun "R_ReplDLLdo1" :int)

