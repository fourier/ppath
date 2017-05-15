;;;; attributes-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.
;;

(in-package :cl-user)
(defpackage py.path.test.nt-test
  (:use :cl
        :py.path.details.nt
        :pypath.test.base
        :prove)
  (:shadowing-import-from py.path.details.nt splitdrive splitunc))
(in-package :py.path.test.nt-test)

;;(shadowing-import 'py.path.details.nt::splitdrive)

;; NOTE: To run this test file, execute `(asdf:test-system :pypath)' in your Lisp.

(plan nil)

  
(subtest "Test splitdrive"
  (test-input splitdrive "C:\\" '("C:" . "\\"))
  (test-input splitdrive "C:\\Sources\\lisp" '("C:" . "\\Sources\\lisp"))
  (test-input splitdrive "C:/Sources/lisp" '("C:" . "/Sources/lisp"))
  (test-input splitdrive "//host-name/share-name/dir" '("//host-name/share-name" . "/dir"))
  (test-input splitdrive "\\\\host-name\\share-name\\dir" '("\\\\host-name\\share-name" . "\\dir"))
  (test-input splitdrive "dir1\\dir2" '("" . "dir1\\dir2")))


(subtest "Test split"
  (test-input split "c:\\Sources\\lisp" '("c:\\Sources" . "lisp"))
  (test-input split "\\\\host-name\\share-name\\dir1\\dir2"
              '("\\\\host-name\\share-name\\dir1" . "dir2"))
  (test-input split "c:\\" '("c:\\" . ""))
  (test-input split "\\\\host-name\\share-name\\" '("\\\\host-name\\share-name\\" . ""))
  (test-input split "c:/" '("c:/" . ""))
  (test-input split "//host-name/share-name/" '("//host-name/share-name/" . ""))
  (test-input split "//host-name/share-name" '("//host-name/share-name" . "")))


(subtest "Test splitunc"
  (test-input splitunc "c:\\Sources\\lisp" '("" . "c:\\Sources\\lisp"))
  (test-input splitunc "c:/Sources/lisp" '("" . "c:/Sources/lisp"))
  (test-input splitunc "\\\\host-name\\share-name\\Sources\\lisp"
              '("\\\\host-name\\share-name" . "\\Sources\\lisp"))
  (test-input splitunc "//host-name/share-name/Sources/lisp"
              '("//host-name/share-name" . "/Sources/lisp"))
  (test-input splitunc "\\\\\\host-name\\share-name\\Sources\\lisp"
              '("" . "\\\\\\host-name\\share-name\\Sources\\lisp"))
  (test-input splitunc "///host-name/share-name/Sources/lisp"
              '("" . "///host-name/share-name/Sources/lisp"))
  (test-input splitunc "\\\\host-name\\\\share-name\\Sources\\lisp"
              '("" . "\\\\host-name\\\\share-name\\Sources\\lisp"))
  (test-input splitunc "//host-name//share-name/Sources/lisp"
              '("" . "//host-name//share-name/Sources/lisp")))


(finalize)
