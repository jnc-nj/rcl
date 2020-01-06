;; Copyright (c) 2006-2013 Carlos Ungil

;; http://cran.r-project.org/doc/manuals/R-ints.html#Attributes

(in-package :rcl)

(defvar *debug-attributes* nil
  "Print to standard output details about attributes when encountered")

(defvar *r-attributes-prefix* ";R. "
  "Default prefix used to print attributes")

(defun print-attributes (attributes sexp)
  (let ((*print-pretty* nil))
    (format t 
	    (concatenate 'string  *r-attributes-prefix* "Attributes for ~A:~&~{"  *r-attributes-prefix* "~S~&~}")
	    (r-obj-describe sexp) attributes)))

(defun attributes-list (attributes)
  (when attributes
    (cond
      ((stringp attributes)
       (list (list attributes)))
      ((= (length attributes) 3)
       (destructuring-bind (attribute-value more-attributes attribute-name) attributes
	 (unless (typep attribute-name 'r-symbol)
	   (error "I expected a symbol, I got ~A" attribute-name))
	 (append (list (cons (intern (string-upcase (name attribute-name)) "KEYWORD")
			     attribute-value))
		 (attributes-list more-attributes))))
      (t (error "These is not a three element list or a string : ~A" attributes)))))

(defun decode-attributes (sexp)
  (let ((attributes (sexp-attrib sexp)))
    (if (member (r-type attributes) '(:list-of-dotted-pairs :scalar-string-type :null))
	(let ((attributes (attributes-list (r-to-lisp attributes))))
	  (when (and *debug-attributes* attributes)
	    (print-attributes attributes sexp))
	  attributes)
	(error "I was expecting the attributes sexp to be :list-of-dotted-pairs or :scalar-string-type, instead of ~A (~S)" 
	       (r-type attributes) (r-to-lisp attributes)))))
