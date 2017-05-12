;;;; attributes-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.
;;

(in-package :cl-user)
(defpackage py.path.test.nt-test
  (:use :cl
        :py.path.details.nt
        :prove))
(in-package :py.path.test.nt-test)


;; NOTE: To run this test file, execute `(asdf:test-system :pypath)' in your Lisp.

(plan nil)

(subtest "Test splitdrive"
  (is (py.path.details.nt::splitdrive "C:\\")
      '("C:" . "\\")
	  :test #'equal
      "Testing input: 'C:\\'"))


(finalize)
