;;;; package.lisp -*- Mode: Lisp;-*- 

(cl:in-package :cl-user) 


(defpackage :relative-package-names
  (:use :cl)) 


(defpackage :package-tests
  (:use #:common-lisp #:ptester)
  (:import-from #:relative-package-names #:package-children)
  (:import-from #:relative-package-names #:package-parent)) 


;;; *EOF*
