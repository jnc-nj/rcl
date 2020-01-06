;; Copyright (c) 2006-2015 Carlos Ungil

(in-package :cl-user)

(defpackage "RCL"
  (:nicknames "R") 
  (:use "CL")
  (:export "R-INIT" "R-QUIT" "ENSURE-R"
	   "R" "R%" "R-PRINT" "R-SUMMARY"
	   "*R-STREAMS*"
	   "R%-PARSE-EVAL" "R-PARSE-EVAL"
	   "*DEBUG-ATTRIBUTES*" "*DOWNCASE-ARGNAMES*"
	   "WITH-DEVICE" "WITH-PAR" "X11" "*USE-QUARTZ*"
	   "ENABLE-RCL-SYNTAX"
	   "R-TO-LISP"
	   "NEW-STRING" "NEW-STRING-SINGLE"))

(defpackage "RCL-TEST"
  (:use "CL" "RCL")
  (:export "RUN"))

