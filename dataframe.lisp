;; Copyright (c) 2006-2014 Carlos Ungil

(in-package :rcl)

;; (r "as.data.frame" (r% "matrix" 1 2 3 :dimnames '(("a" "b") ("X" "Y" "Z"))))

(defclass r-dataframe ()
  ((data :accessor data :initarg :data)
   (names :accessor names :initarg :names)
   (rownames :accessor rownames :initarg :rownames)))

(defmethod print-object ((dataframe r-dataframe) stream) 
  (print-unreadable-object (dataframe stream :type t :identity t)
    (format stream "~s rows, ~s columns, column names ~{~s~^ ~}" (length (first (data dataframe))) (length (data dataframe)) (names dataframe))))
