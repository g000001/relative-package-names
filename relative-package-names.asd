;;;; relative-package-names.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf) 


(defsystem :relative-package-names
  :serial t
  :depends-on (:ptester)
  :components ((:file "package")
               (:file "relative-package-names")
               (:file "test"))
  :in-order-to ((test-op (load-op :relative-package-names)))
  :perform (test-op (op c)
             (or (funcall (intern (string :run-tests) :package-tests))
                 (error "test-op failed") ))) 


(defmethod perform :after ((o test-op) (c (eql (find-system :relative-package-names))))
  (dolist (p '(:package-tests.a
               :package-tests.a.b
               :package-tests.a.b.c
               :package-tests.a.b.c.d
               :package-tests.a.b.c.d.e
               :package-tests.a.b.c.d.f
               :package-tests.a.b.c.e
               :package-tests.a.b.c.f
               :package-tests.a.b.d
               :package-tests.a.b.e
               :package-tests.a.c
               :package-tests.a.d
               :package-tests.b
               :package-tests.c
               :package-tests.d
               :package-tests-foo.bar.baz
               :package-tests-foo.bar.baz.wham))
    (ignore-errors (delete-package p)))) 


;;; *EOF* 


