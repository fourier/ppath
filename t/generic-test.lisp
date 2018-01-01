;;;; generic-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :ppath)' in your Lisp.
;;

(in-package :cl-user)
(defpackage ppath.test.generic-test
  (:use :cl
        :alexandria
        :ppath.test.base
        :prove)
  (:shadowing-import-from ppath.details.generic
   concat commonprefix splitext))

(in-package :ppath.test.generic-test)

;; NOTE: To run this test file, execute `(asdf:test-system :ppath)' in your Lisp.

(plan nil)


(subtest "Test commonprefix"
  (test-input commonprefix nil "")
  (test-input commonprefix '("/home/username/dir" "/home/user/test") "/home/user")
  (test-input commonprefix '("/home/user/dir" "/home/user/test") "/home/user/")
  (test-input commonprefix '("/home/user/dir" "/home/user/dir") "/home/user/dir")
  (test-input commonprefix '("home:username:dir" "home:user:dir") "home:user")
  (test-input commonprefix '(":home:user:dir" ":home:user:test") ":home:user:")
  (test-input commonprefix '(":home:user:dir" ":home:user:dir") ":home:user:dir")
  (let ((testlist '("" "abc" "Xbcd" "Xb" "XY" "abcd"
                    "aXc" "abd" "ab" "aX" "abcX")))
    (loop for s1 in testlist do
          (loop for s2 in testlist
                for p = (commonprefix s1 s2)
                for n = (length p)
                do
                (ok (starts-with-subseq p s1)
                    (format nil "~s starts with ~s" s1 p))
                (ok (starts-with-subseq p s2)
                    (format nil "~s starts with ~s" s2 p))
                (when (string-not-equal s1 s2)
                  (let ((ss1 (subseq s1 n (min (1+ n) (length s1))))
                        (ss2 (subseq s2 n (min (1+ n) (length s2)))))
                  (ok (string-not-equal ss1 ss2)
                      (format nil "~s != ~s" ss1 ss2))))))))

(defmacro splitext-test (path fname ext)
  `(progn
    (test-input splitext ,path '(,fname . ,ext))
    (test-input splitext ,(concat "/" path)
                '(,(concat "/" fname) . ,ext))
    (test-input splitext ,(concat "abc/" path) '(,(concat "abc/" fname) . ,ext))
    (test-input splitext ,(concat "abc.def/" path)  '(,(concat "abc.def/" fname) . ,ext))
    (test-input splitext ,(concat "abc.def/" path) '(,(concat "abc.def/"  fname) . ,ext))
    (test-input splitext ,(concat "/abc.def/" path) '(,(concat "/abc.def/" fname) . ,ext))
    (test-input splitext ,(concat path "/") '(,(concat fname ext "/") . ""))
))

;; test nt splitext
#+:windows
(subtest "Test splitext for nt"
  (test-input splitext "foo.ext" '("foo" . ".ext"))
  (test-input splitext "/foo/foo.ext" '("/foo/foo" . ".ext"))
  (test-input splitext ".ext" '(".ext" . ""))
  (test-input splitext "\\foo.ext\\foo" '("\\foo.ext\\foo" . ""))
  (test-input splitext "foo.ext\\" '("foo.ext\\" . ""))
  (test-input splitext "" '("" . ""))
  (test-input splitext "foo.bar.ext" '("foo.bar" . ".ext"))
  (test-input splitext "xx/foo.bar.ext" '("xx/foo.bar" . ".ext"))
  (test-input splitext "xx\\foo.bar.ext" '("xx\\foo.bar" . ".ext"))
  (test-input splitext "c:a/b\\c.d" '("c:a/b\\c" . ".d")))
;; test posix splitext
(subtest "Test splitext for posix"
  (splitext-test "foo.bar" "foo" ".bar")
  (splitext-test "foo.boo.bar" "foo.boo" ".bar")
  (splitext-test "foo.boo.biff.bar" "foo.boo.biff" ".bar")
  (splitext-test ".csh.rc" ".csh" ".rc")
  (splitext-test "nodots" "nodots" "")
  (splitext-test ".cshrc" ".cshrc" "")
  (splitext-test "...manydots" "...manydots" "")
  (splitext-test "...manydots.ext" "...manydots" ".ext")
  (splitext-test "." "." "")
  (splitext-test ".." ".." "")
  (splitext-test "........" "........" "")
  (splitext-test "" "" ""))


(finalize)
