;;;; test.lisp -*- Mode: Lisp;-*- 


;; The following source code is in the public domain.
;; Provided "as is" with no warranty of any kind.  
;; Use at your own risk.
;;; http://franz.com/support/documentation/9.0/doc/packages.htm

 
(cl:in-package :package-tests) 


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *package-error-condition* #+sbcl 'sb-kernel:simple-package-error)) 


(defun do-package-tests ()
  (test t
	(progn #+relative-package-names t
	       #-relative-package-names nil)
	:test #'eq)
  
;;;; test package-children
  (test '(:package-tests.a :package-tests.b
	  :package-tests.c :package-tests.d)
	(sort (mapcar (lambda (s) (intern (package-name s) :keyword))
		      (package-children :package-tests :recurse nil))
	      #'string<)
	:test #'equal)
  (test '(:package-tests.a :package-tests.a.b :package-tests.a.b.c
	  :package-tests.a.b.c.d :package-tests.a.b.c.d.e
	  :package-tests.a.b.c.d.f :package-tests.a.b.c.e
	  :package-tests.a.b.c.f :package-tests.a.b.d :package-tests.a.b.e
	  :package-tests.a.c :package-tests.a.d :package-tests.b
	  :package-tests.c :package-tests.d)
	(sort (mapcar (lambda (s) (intern (package-name s) :keyword)) (package-children ':package-tests))
	      #'string<)
	:test #'equal)
  (test '(:package-tests.a.b.c.d :package-tests.a.b.c.d.e
	  :package-tests.a.b.c.d.f :package-tests.a.b.c.e
	  :package-tests.a.b.c.f)
	(sort (mapcar (lambda (s) (intern (package-name s) :keyword)) (package-children ':package-tests.a.b.c))
	      #'string<)
	:test #'equal)
  (test '(:package-tests.a.b.c.d :package-tests.a.b.c.e
	  :package-tests.a.b.c.f)
	(sort (mapcar (lambda (s) (intern (package-name s) :keyword))
		      (package-children ':package-tests.a.b.c :recurse nil))
	      #'string<)
	:test #'equal)
  (test '(:package-tests.a.b.c.d.e :package-tests.a.b.c.d.f)
	(sort (mapcar (lambda (s) (intern (package-name s) :keyword)) (package-children ':package-tests.a.b.c.d))
	      #'string<)
	:test #'equal)
  (test '(:package-tests.a.b.c.d.e :package-tests.a.b.c.d.f)
	(sort (mapcar (lambda (s) (intern (package-name s) :keyword))
		      (package-children :package-tests.a.b.c.d :recurse nil))
	      #'string<)
	:test #'equal)
  (test '()
	(package-children :package-tests.b)
	:test #'equal)
  (test '()
	(package-children :package-tests.c)
	:test #'equal)
  (test '()
	(package-children :package-tests.d)
	:test #'equal)
  
;;;; test package-parent
  (test (find-package :package-tests)
	(package-parent :package-tests.a))
  (test (find-package :package-tests.a)
	(package-parent :package-tests.a.b))
  (test (find-package :package-tests.a.b)
	(package-parent :package-tests.a.b.c))
  (test (find-package :package-tests.a.b.c)
	(package-parent :package-tests.a.b.c.d))
  (test (find-package :package-tests.a.b.c.d)
	(package-parent :package-tests.a.b.c.d.e))
  (test (find-package :package-tests.a.b.c.d)
	(package-parent :package-tests.a.b.c.d.f))
  (test (find-package :package-tests.a.b.c)
	(package-parent :package-tests.a.b.c.e))
  (test (find-package :package-tests.a.b.c)
	(package-parent :package-tests.a.b.c.f))
  (test (find-package :package-tests.a.b)
	(package-parent :package-tests.a.b.d))
  (test (find-package :package-tests.a.b)
	(package-parent :package-tests.a.b.e))
  (test (find-package :package-tests.a)
	(package-parent :package-tests.a.c))
  (test (find-package :package-tests.a)
	(package-parent :package-tests.a.d))
  (test (find-package :package-tests)
	(package-parent :package-tests.b))
  (test (find-package :package-tests)
	(package-parent :package-tests.c))
  (test (find-package :package-tests)
	(package-parent :package-tests.d))

  (test-error (package-parent :package-tests) :condition-type *package-error-condition*)
  (test-error (package-parent :package-tests-foo.bar.baz) :condition-type *package-error-condition*)
  (test-error (package-parent :package-tests-foo.bar) :condition-type *package-error-condition*)
  (test-error (package-parent :package-tests-foo) :condition-type *package-error-condition*)
  
;;;; test find-package
  (dolist
      (item
	  '((:package-tests.a         :package-tests.a :.)
	    (:package-tests           :package-tests.a :..)
	    (:package-tests.b         :package-tests.a :..b)
	    (:package-tests.c         :package-tests.a :..c)
	    (:package-tests.d         :package-tests.a :..d)
	    (:package-tests.a.b       :package-tests.b :..a.b)
	    (:package-tests           :package-tests.a.b :...)
	    (:package-tests.b         :package-tests.a.b :...b)
	    (:package-tests.a.b.c.d.f :package-tests.a.b.c.d :...c.d.f)
	    (:package-tests           :package-tests.a.b.c.d :.....)
	    (:package-tests.b         :package-tests.a.b.c.d :.....b)
	    (:package-tests.a.b.c.d   :package-tests.a.b.c.d :.)
	    (:package-tests.a.b.c     :package-tests.a.b.c :.)
	    (:package-tests.a.b       :package-tests.a.b :.)))
    (test (symbol-name (first item))
	  (let* ((*package* (find-package (second item)))
		 (p (find-package (third item))))
	    (cond (p (package-name p))
		  (t (error "could not find package ~s." (third item)))))
          :test #'string=))

  (let ((*package* (find-package :package-tests)))
    (test-error (find-package :..) :condition-type *package-error-condition*)
    (test-error (find-package :...) :condition-type *package-error-condition*)
    (test-error (find-package :....) :condition-type *package-error-condition*)
    (test-error (find-package :....foo) :condition-type *package-error-condition*))
  (let ((*package* (find-package :package-tests.b)))
    (test-error (find-package :...) :condition-type *package-error-condition*))) 


(defun run-tests ()
  (defpackage :package-tests.a) 
  (defpackage :package-tests.a.b) 
  (defpackage :package-tests.a.b.c)
  (defpackage :package-tests.a.b.c.d)
  (defpackage :package-tests.a.b.c.d.e)
  (defpackage :package-tests.a.b.c.d.f)
  (defpackage :package-tests.a.b.c.e)
  (defpackage :package-tests.a.b.c.f)
  (defpackage :package-tests.a.b.d)
  (defpackage :package-tests.a.b.e)
  (defpackage :package-tests.a.c)
  (defpackage :package-tests.a.d)
  (defpackage :package-tests.b)
  (defpackage :package-tests.c)
  (defpackage :package-tests.d)
  (defpackage :package-tests-foo.bar.baz)
  (defpackage :package-tests-foo.bar.baz.wham) 
  (let ((*test-errors* 0)
        (*test-successes* 0)
        (*test-unexpected-failures* 0))
    (format t "Beginning package tests...~%")
    (do-package-tests)
    (format t "Completed package tests.~%")
    (format t "** Successes: ~s~%" *test-successes*)
    (format t "** Errors: ~s~%" *test-errors*)
    (format t "** Unexpected failures: ~s~%" *test-unexpected-failures*)
    (zerop *test-unexpected-failures*))) 


;;; *EOF* 

