(in-package :rcl-test)

(defun run ()
  (5am:run! 'rcl-test::rcl-suite))

(5am:def-suite rcl-suite)

(5am:in-suite rcl-suite)

(5am:test init
  (5am:is (eq :running (r-init))))

(5am:test (sum :depends-on init)
  (5am:is (equal '(4) (r "+" 2 2))))

(5am:test (sum2 :depends-on init)
  (5am:is (equal '(22 23 24) (r "+" 20 '(2 3 4)))))

(5am:test (new-string-single :depends-on init)
  (5am:is (equal '("eo") (r-to-lisp (new-string-single "eo")))))

(5am:test (new-string :depends-on init)
  (5am:is (equal (loop repeat 10 collect "") (r-to-lisp (new-string 10)))))

(5am:test (list :depends-on init)
  (5am:is (equal (r "list" '("A" "B") '("C" "D")) '(("A" "B") ("C" "D")))))

(5am:test (matrix :depends-on init)
  (5am:is (equalp (rcl::matrix (r "matrix" '(1 2 3 4) :nrow 2 :byrow t))
		  #2A((1 2) (3 4)))))

;; (5am:test matrix2
;;   (5am:is (and (equal (rcl::names (r "matrix" '(1 2 3 4) :nrow 2 :dimnames '(("A" "B") ("C" "D"))))
;; 		      '(("A" "B") ("C" "D")))
;; 	       (equalp (rcl::matrix (r "matrix" '(1 2 3 4) :nrow 2 :dimnames '(("A" "B") ("C" "D"))))
;; 		       #2A((1 3) (2 4))))))

(5am:test (gc :depends-on init)
  (let ((r:*r-streams* :console))
    (5am:is (numberp (reduce #'+ (loop repeat 1000 append (rcl:r "runif" 1)))))))

(5am:test (x11 :depends-on init)
  (5am:is-true (progn (x11)
		      #-bordeaux-threads t
		      #+bordeaux-threads (bt:thread-alive-p r::*event-loop*))))

(5am:test (plot :depends-on x11)
  (5am:is-false (r "plot" (r% "rnorm" 20) (r% "rnorm" 20) :xlab "" :ylab "")))

;; restarting is not supported by R
#+NIL
(5am:test init-and-quit-twice 
  (5am:is (r:r-init))
  (5am:is (null (r:r-quit)))
  (5am:is (r:r-init))
  (5am:is (null (r:r-quit))))
