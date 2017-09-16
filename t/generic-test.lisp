;;;; generic-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :pypath)' in your Lisp.
;;

(in-package :cl-user)
(defpackage py.path.test.generic-test
  (:use :cl
        :alexandria
        :pypath.test.base
        :prove)
  (:shadowing-import-from py.path.details.generic
   commonprefix))

(in-package :py.path.test.generic-test)

;; NOTE: To run this test file, execute `(asdf:test-system :pypath)' in your Lisp.

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

(subtest "Test splitext"
  #|
          tester('ntpath.splitext("foo.ext")', ('foo', '.ext'))
        tester('ntpath.splitext("/foo/foo.ext")', ('/foo/foo', '.ext'))
        tester('ntpath.splitext(".ext")', ('.ext', ''))
        tester('ntpath.splitext("\\foo.ext\\foo")', ('\\foo.ext\\foo', ''))
        tester('ntpath.splitext("foo.ext\\")', ('foo.ext\\', ''))
        tester('ntpath.splitext("")', ('', ''))
        tester('ntpath.splitext("foo.bar.ext")', ('foo.bar', '.ext'))
        tester('ntpath.splitext("xx/foo.bar.ext")', ('xx/foo.bar', '.ext'))
        tester('ntpath.splitext("xx\\foo.bar.ext")', ('xx\\foo.bar', '.ext'))
        tester('ntpath.splitext("c:a/b\\c.d")', ('c:a/b\\c', '.d'))

    def splitextTest(self, path, filename, ext):
        self.assertEqual(posixpath.splitext(path), (filename, ext))
        self.assertEqual(posixpath.splitext("/" + path), ("/" + filename, ext))
        self.assertEqual(posixpath.splitext("abc/" + path), ("abc/" + filename, ext))
        self.assertEqual(posixpath.splitext("abc.def/" + path), ("abc.def/" + filename, ext))
        self.assertEqual(posixpath.splitext("/abc.def/" + path), ("/abc.def/" + filename, ext))
        self.assertEqual(posixpath.splitext(path + "/"), (filename + ext + "/", ""))

    def test_splitext(self):
        self.splitextTest("foo.bar", "foo", ".bar")
        self.splitextTest("foo.boo.bar", "foo.boo", ".bar")
        self.splitextTest("foo.boo.biff.bar", "foo.boo.biff", ".bar")
        self.splitextTest(".csh.rc", ".csh", ".rc")
        self.splitextTest("nodots", "nodots", "")
        self.splitextTest(".cshrc", ".cshrc", "")
        self.splitextTest("...manydots", "...manydots", "")
        self.splitextTest("...manydots.ext", "...manydots", ".ext")
        self.splitextTest(".", ".", "")
        self.splitextTest("..", "..", "")
        self.splitextTest("........", "........", "")
        self.splitextTest("", "", "")
|#
  )      

(finalize)
