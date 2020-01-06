;; Copyright (c) 2006-2015 Carlos Ungil

(in-package :rcl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *r-home* 
    #+darwin "/Library/Frameworks/R.framework/Resources/"
    ;; homebrew alternative: brew install R --with-openblas --with-x11
    ;; #+darwin "/usr/local/Cellar/r/3.3.1_2/R.framework/Resources/" 
    #+windows "C:\\Program Files\\R\\R-3.3.1\\";;; ABCL won't work with forward slashes
    #-(or darwin windows) "/usr/lib/R/")
  (defvar *r-lib-name*
    #+windows "R"
    #-windows "libR")
  (defvar *r-lib-extension*
    #+windows ".dll"
    #+darwin ".dylib"
    #-(or darwin windows) ".so")
  (defvar *r-lib-path* 
    (concatenate 'string *r-home*
		 #+(and windows ecl) "bin\\i386\\"
		 #+(and windows (not ecl) cffi-features:x86) "bin\\i386\\"
                 #+(and windows (not ecl) cffi-features:x86-64) "bin\\x64\\"
		 #-windows "lib/"))
  ;; CFFI re-introduces forward slashes in the path when searching
  ;; As a workaround, we give the full path to load-foreign-library
  (defvar *r-lib* 
    #-(and abcl windows) (concatenate 'string *r-lib-name* *r-lib-extension*)
    #+(and abcl windows) (concatenate 'string *r-lib-path* *r-lib-name* *r-lib-extension*)))





