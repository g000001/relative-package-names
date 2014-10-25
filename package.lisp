;;;; package.lisp -*- Mode: Lisp;-*- 

(cl:in-package :cl-user) 


(defpackage :relative-package-names
  (:use :cl)
  (:intern #:package-children #:package-parent)) 


(defpackage :package-tests
  (:use #:common-lisp #:ptester)
  (:import-from #:relative-package-names #:package-children #:package-parent)) 


;;; *EOF*
