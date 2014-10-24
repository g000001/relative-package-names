;;;; relative-package-names.lisp -*- Mode: Lisp;-*- 

;; The following source code is in the public domain.
;; Provided "as is" with no warranty of any kind.  
;; Use at your own risk.
;;; http://franz.com/support/documentation/9.0/doc/packages.htm

(cl:in-package :relative-package-names) 


(deftype index () 'unsigned-byte) 


(defun mark-supported ()
  (pushnew :relative-package-names *features*))


;;; package-name-to-package  --  Internal
;;;
;;; Given a package name, a simple-string, do a package name lookup.
;;;
(defun package-name-to-package (name)
  (declare (simple-string name))
  #+(:or :sbcl) (values (gethash name sb-impl::*package-names*))
  #-(:or :sbcl) (values (gethash name *package-names*))) 


;;; package-namestring  --  Internal
;;;
;;;    Take a package-or-string-or-symbol and return a package name.
;;;
(defun package-namestring (thing)
  (if (packagep thing)
      (let ((name (sb-impl::package-%name thing)))
	(or name
	    (error "Can't do anything to a deleted package: ~S" thing)))
      (sb-impl::package-namify thing))) 


;;; package-parent  --  Internal.
;;;
;;; Because this function is called via the reader, we want it to be as
;;; fast as possible.
;;;
(defun package-parent (package-specifier)
  "Given PACKAGE-SPECIFIER, a package, symbol or string, return the
  parent package.  If there is not a parent, signal an error."
  (declare (optimize (speed 3)))
  (flet ((find-last-dot (name)
           (do* ((len (1- (length name)))
		 (i len (1- i)))
		((= i -1) nil)
             (declare (fixnum len i))
             (when (char= #\. (schar name i)) (return i)))))
    (let* ((child (package-namestring package-specifier))
           (dot-position (find-last-dot child)))
      (declare (string child))
      (cond (dot-position
             (let ((parent (subseq child 0 dot-position)))
               (or (package-name-to-package parent)
		   (error 'sb-kernel:simple-package-error
                          :name child
                          :format-control "The parent of ~a does not exist."
                          :format-arguments (list child)))))
            (t
	     (error 'sb-kernel:simple-package-error
                    :name child
                    :format-control "There is no parent of ~a."
                    :format-arguments (list child))))))) 


;;; package-children  --  Internal.
;;;
;;; While this function is not called via the reader, we do want it to be
;;; fast.
;;;
(defun package-children (package-specifier &key (recurse t))
  "Given PACKAGE-SPECIFIER, a package, symbol or string, return all the
  packages which are in the hierarchy 'under' the given package.  If
  :recurse is nil, then only return the immediate children of the package."
  (declare (optimize (speed 3)))
  (let ((res ())
        (parent (package-namestring package-specifier)))
    (labels
        ((string-prefix-p (prefix string)
	   (declare (simple-string prefix string))
           ;; Return length of `prefix' if `string' starts with `prefix'.
           ;; We don't use `search' because it does much more than we need
           ;; and this version is about 10x faster than calling `search'.
           (let ((prefix-len (length prefix))
                 (seq-len (length string)))
             (declare (type index prefix-len seq-len))
             (when (>= prefix-len seq-len)
               (return-from string-prefix-p nil))
             (do ((i 0 (1+ i)))
                 ((= i prefix-len) prefix-len)
               (declare (type index i))
               (unless (char= (schar prefix i) (schar string i))
                 (return nil)))))
         (test-package (package-name package)
	   (declare (simple-string package-name)
		    (type package package))
           (let ((prefix
                  (string-prefix-p (concatenate 'simple-string parent ".")
                                   package-name)))
             (cond (recurse
		    (when prefix
		      (pushnew package res)))
                   (t
		    (when (and prefix
			       (not (find #\. package-name :start prefix)))
		      (pushnew package res)))))))
      #+(:or :sbcl) (maphash #'test-package sb-impl::*package-names*)
      #-(:or :sbcl) (maphash #'test-package *package-names*)
      res))) 


;;; relative-package-name-to-package  --  Internal
;;;
;;; Given a package name, a simple-string, do a relative package name lookup.
;;; It is intended that this function will be called from find-package.
;;;
(defun relative-package-name-to-package (name)
  (declare (simple-string name)
	   (optimize (speed 3)))
  (flet ((relative-to (package name)
	   (declare (type package package)
		    (simple-string name))
           (if (string= "" name)
	       package
	       (let ((parent-name (sb-impl::package-%name package)))
		 (unless parent-name
		   (error "Can't do anything to a deleted package: ~S"
			  package))
		 (package-name-to-package
		  (concatenate 'simple-string parent-name "." name)))))
         (find-non-dot (name)
           (do* ((len (length name))
                 (i 0 (1+ i)))
		((= i len) nil)
             (declare (type index len i))
             (when (char/= #\. (schar name i)) (return i)))))
    (when (and (plusp (length name))
	       (char= #\. (schar name 0)))
      (let* ((last-dot-position (or (find-non-dot name) (length name)))
             (n-dots last-dot-position)
             (name (subseq name last-dot-position)))
        (cond ((= 1 n-dots)
               ;; relative to current package
               (relative-to *package* name))
              (t
               ;; relative to our (- n-dots 1)'th parent
               (let ((package *package*)
                     (tmp nil))
                 (dotimes (i (1- n-dots))
		   (declare (fixnum i))
		   (setq tmp (package-parent package))
                   (unless tmp
		     (error 'sb-kernel:simple-package-error
                            :name (package-namestring package)
                            :format-control "The parent of ~a does not exist."
                            :format-arguments (list package)))
                   (setq package tmp))
                 (relative-to package name)))))))) 


;;; find-package  --  Public
;;;
;;;
(sb-ext:without-package-locks 
  (defun cl:find-package (name)
    "Find the package having the specified name."
    (if (packagep name)
        name
        (let ((name (sb-impl::package-namify name)))
          (or (package-name-to-package name)
              (relative-package-name-to-package name)))))) 


(mark-supported)


;;; *EOF* 


