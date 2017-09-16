;;;; posix-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :pypath)' in your Lisp.
;;

(in-package :cl-user)
(defpackage py.path.test.posix-test
  (:use :cl
        :alexandria
        :pypath.test.base
        :prove))
;;;   (:shadowing-import-from py.path.details.posix
;;;    commonprefix))

(in-package :py.path.test.posix-test)

;; NOTE: To run this test file, execute `(asdf:test-system :pypath)' in your Lisp.

(plan nil)


;;; (subtest "Test commonprefix"
;;;   (test-input commonprefix nil "")
;;;   (test-input commonprefix '("/home/username/dir" "/home/user/test") "/home/user")
;;;   (test-input commonprefix '("/home/user/dir" "/home/user/test") "/home/user/")
;;;   (test-input commonprefix '("/home/user/dir" "/home/user/dir") "/home/user/dir")
;;;   (test-input commonprefix '("home:username:dir" "home:user:dir") "home:user")
;;;   (test-input commonprefix '(":home:user:dir" ":home:user:test") ":home:user:")
;;;   (test-input commonprefix '(":home:user:dir" ":home:user:dir") ":home:user:dir")
;;;   (let ((testlist '("" "abc" "Xbcd" "Xb" "XY" "abcd"
;;;                     "aXc" "abd" "ab" "aX" "abcX")))
;;;     (loop for s1 in testlist do
;;;           (loop for s2 in testlist
;;;                 for p = (commonprefix s1 s2)
;;;                 for n = (length p)
;;;                 do
;;;                 (ok (starts-with-subseq p s1)
;;;                     (format nil "~s starts with ~s" s1 p))
;;;                 (ok (starts-with-subseq p s2)
;;;                     (format nil "~s starts with ~s" s2 p))
;;;                 (when (string-not-equal s1 s2)
;;;                   (let ((ss1 (subseq s1 n (min (1+ n) (length s1))))
;;;                         (ss2 (subseq s2 n (min (1+ n) (length s2)))))
;;;                   (ok (string-not-equal ss1 ss2)
;;;                       (format nil "~s != ~s" ss1 ss2))))))))

(finalize)
