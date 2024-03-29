;;;; posix-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :ppath)' in your Lisp.
;;

(in-package :cl-user)
(defpackage ppath.test.posix-test
  (:use :cl
        :alexandria
        :ppath.test.base
        :prove)
   (:shadowing-import-from :ppath.details.generic getenv concat)
   (:shadowing-import-from :ppath.details.posix 
    join
    split
    isabs
    basename
    dirname
    islink
    sameopenfile
    samefile
    exists
    lexists
    getpwuid ; helper function
    getuid   ; helper function
    getpwnam ; helper function   
    expanduser
    expandvars
    normpath
    abspath
    relpath))

(in-package :ppath.test.posix-test)

;; NOTE: To run this test file, execute `(asdf:test-system :ppath)' in your Lisp.

(plan nil)


(subtest "Test join"
  (test-input join '("/foo" "bar" "/bar" "baz") "/bar/baz")
  (test-input join '("/foo" "bar" "baz") "/foo/bar/baz")
  (test-input join '("/foo/" "bar/" "baz/") "/foo/bar/baz/")
  (test-input join '("/" "/") "/")
  (test-input join '("a" "/bb") "/bb"))
  

(subtest "Test split"
  (test-input split "" '("" . ""))
  (test-input split "/foo/bar" '("/foo" . "bar"))
  (test-input split "/"  '("/" . ""))
  (test-input split "foo" '("" . "foo"))
  (test-input split "////foo" '("////" . "foo"))
  (test-input split "//foo//bar" '("//foo" . "bar"))
  (test-input split "/foo/bar" '("/foo" . "bar"))
  (test-input split "/foo/bar/" '("/foo/bar/" . "")))

(subtest "Test isabs"
  (test-input isabs "" nil)
  (test-input isabs "/" t)
  (test-input isabs "/foo" t)
  (test-input isabs "/foo/bar" t)
  (test-input isabs "foo/bar" nil))

(subtest "Test basename"
  (test-input basename "/foo/bar" "bar")
  (test-input basename "/" "")
  (test-input basename "foo" "foo")
  (test-input basename "////foo" "foo")
  (test-input basename "//foo//bar" "bar"))

(subtest "Test dirname"
  (test-input dirname "/foo/bar" "/foo")
  (test-input dirname "/" "/")
  (test-input dirname "foo" "")
  (test-input dirname "////foo" "////")
  (test-input dirname "//foo//bar" "//foo"))

(defun create-temp-filename ()
  (osicat-posix:mktemp (join (ppath.details.generic:get-temp-path) "test-ppath-XXXXXX")))

(subtest "Test exists/lexists"
  (let ((file1 (create-temp-filename))
        (file2 (create-temp-filename)))
    (with-open-file (f file1 :direction :output :if-exists :supersede)
      (write "test string" :stream f))
    (ok (exists file1) (format nil "Test ~s exists" file1))
    (ok (not (exists file2)) (format nil "Test ~s doesn't exist" file2))
    (osicat-posix:symlink file1 file2)
    (ok (exists file2) (format nil "Now test ~s exists" file2))
    (ok (lexists file2) (format nil "Now test ~s lexists" file2))
    (delete-file file1)
    (ok (not (exists file2)) (format nil "Source removed, test if link is broken ~s" file2))
    (ok (lexists file2) (format nil "Now test ~s lexists" file2))
    (delete-file file2)))


(subtest "Test islink"
  (let ((file1 (create-temp-filename))
        (file2 (create-temp-filename)))
    (with-open-file (f file1 :direction :output :if-exists :supersede)
      (write "test string" :stream f))
    (osicat-posix:symlink file1 file2)
    (ok (not (islink file1)) (format nil "Test ~s is not link" file1))
    (ok (islink file2) (format nil "Test ~s is link" file2))
    (delete-file file1)
    (delete-file file2)))

(subtest "Test samefile"
  (let ((file1 (create-temp-filename))
        (file2 (create-temp-filename))
        (file3 (create-temp-filename)))
    (with-open-file (f file1 :direction :output :if-exists :supersede)
      (write "test string" :stream f))
    (with-open-file (f file3 :direction :output :if-exists :supersede)
      (write "test another string" :stream f))
    (osicat-posix:symlink file1 file2)
    (ok (samefile file1 file1) (format nil "Test samefile ~s" file1))
    (ok (samefile file1 file2) (format nil "Test ~s and link ~s are the same" file1 file2))
    (ok (not (samefile file1 file3)) (format nil "Test ~s and ~s are not the same" file1 file3))
    (delete-file file1)
    (delete-file file2)
    (delete-file file3)))


(subtest "Test sameopenfile"
  (let ((file1 (create-temp-filename))
        (file2 (create-temp-filename)))
    (with-open-file (f file1 :direction :output :if-exists :supersede)
      (write "test string" :stream f))
    (with-open-file (f file2 :direction :output :if-exists :supersede)
      (write "test another string" :stream f))
    (with-open-file (f1 file1 :direction :input)
      (with-open-file (f2 file1 :direction :input)
        (with-open-file (f3 file2 :direction :input)
          (ok (sameopenfile f1 f2) (format nil "Test sameopenfile ~s" file1))
          (ok (not (sameopenfile f1 f3)) (format nil "Test not sameopenfile"))
          (delete-file file1)
          (delete-file file2))))))


(subtest "Test expanduser"
  (test-input expanduser "dir" "dir")
  (loop for home in '("/" "//" "///")
        do 
        (with-mocked-function (getenv
                               (lambda (name) 
                                 (ok (string= name "HOME") "Expecting request for HOME environment variable")
                                 home))
          (test-input expanduser "~" "/")
          (test-input expanduser "~/" "/")
          (test-input expanduser "~/dir" "/dir")))
  (with-mocked-function (getenv
                         (lambda (name) 
                           (ok (string= name "HOME") "Expecting request for HOME environment variable")
                           "/home/myhomedir"))
    (test-input expanduser "~/" "/home/myhomedir/")
    (is (concat (expanduser "~") "/") (expanduser "~/") :test #'equal
        "(expandpath \"~/\") should be the same as (concat (expandpath \"~\") \"/\""))
  ;; test the other user home directories
  (with-mocked-function (getpwnam
                         (lambda (name) (cond ((equal name "root") '("root" "***" 1 1 "root" "/root" "/bin/sh"))
                                              ((equal name "user") '("user" "***" 1 1 "root" "/Users/user" "/bin/sh")))))
    (test-input expanduser "~root/" "/root/")
    (test-input expanduser "~user/" "/Users/user/")
    ;; remove HOME variable to test fall-back to getpwuid
    (with-mocked-function (getenv
                           (lambda (name) 
                             (ok (string= name "HOME") "Expecting request for HOME environment variable")
                             nil))
      (with-mocked-function (getuid (lambda () 503))
        (with-mocked-function (getpwuid (lambda (id) (if (= id 503) '("user" "***" 1 1 "root" "/Users/user/" "/bin/sh"))))
          ;; trailing / shall be removed
          (test-input expanduser "~" "/Users/user"))))))
         

(subtest "Test expandvars"
  ;; environment - hash table containing "mocked" environment vars
  (let ((env-vars (make-hash-table :test #'equalp)))
    ;; quickly set mocked environ variable 
    (flet ((env (x y) (setf (gethash x env-vars) y))
           (unenv (x) (remhash x env-vars)))
      ;; mock the getenv function 
      (with-mocked-function (ppath.details.generic::getenv
                              (lambda (name) (gethash name env-vars)))
        (clrhash env-vars)
        (env "foo" "bar")
        (env "{foo" "baz1")
        (env "{foo}" "baz2")
        (test-input expandvars "foo" "foo")
        (test-input expandvars "$foo bar" "bar bar")
        (test-input expandvars "$$foo bar" "$bar bar")
        (test-input expandvars "${foo}bar" "barbar")
        (test-input expandvars "$[foo]bar" "$[foo]bar")
        (test-input expandvars "$bar bar" "$bar bar")
        (test-input expandvars "$?bar" "$?bar")
        (test-input expandvars "$foo}bar" "bar}bar")
        (test-input expandvars "${foo" "${foo")
        (test-input expandvars "${{foo}}" "baz1}")
        (test-input expandvars "$foo$foo" "barbar")
        (test-input expandvars "$bar$bar" "$bar$bar")
        (test-input expandvars "\'$foo\'$bar" "\'bar\'$bar")
        (test-input expandvars "bar\'$foo" "bar\'bar")
        (test-input expandvars '("\'$foo\'$bar" nil) "\'$foo\'$bar")
        (test-input expandvars '("bar\'$foo'" nil) "bar\'$foo'")
        ;; some additional degraded/corner cases
        (test-input expandvars "" "")
        (test-input expandvars "'" "'")
        (test-input expandvars "$" "$")
        (test-input expandvars "%" "%")
        (test-input expandvars "test'" "test'")
        (test-input expandvars "test''" "test''")
        (test-input expandvars "test$$" "test$$")))))


(subtest "Test normpath"
  (test-input normpath "" ".")
  (test-input normpath "/"  "/")
  (test-input normpath "//"  "//")
  (test-input normpath "///"  "/")
  (test-input normpath "dir1/.." ".")
  (test-input normpath "///foo/.//bar//"  "/foo/bar")
  (test-input normpath "///foo/.//bar//.//..//.//baz"  "/foo/baz")
  (test-input normpath "///..//./foo/.//bar"  "/foo/bar")
  (test-input normpath "../.././.." "../../..")
  (test-input normpath "../.././dir//.." "../..")
  (test-input normpath "../dir/.." "..")
  (test-input normpath "../../dir/../dir1" "../../dir1")
  (test-input normpath "/../../dir/../dir1" "/dir1")
  (test-input normpath "../../../dir/../../dir1/.." "../../../..")
  (test-input normpath "../../../dir/../dir1/../dir2" "../../../dir2")
  (test-input normpath "/dir1/.." "/")
  (test-input normpath "dir1/../.." "..")
  (test-input normpath "dir/foo/bar/../.."  "dir")
  (test-input normpath "/dir/foo/bar/../.."  "/dir")
  (test-input normpath "dir/../../foo/bar"  "../foo/bar")
  (test-input normpath "/dir/../../foo/bar"  "/foo/bar"))

(subtest "Test relpath"
  (with-mocked-function (ppath.details.generic::getcwd
                         (lambda () "/home/user/bar"))
    (let ((curdir (cdr (split (ppath.details.generic:getcwd))))
          (abs-a (abspath "a")))
      (test-input relpath "" nil)
      (test-input relpath "a" "a")
      (test-input relpath abs-a "a")
      (test-input relpath "a/b" "a/b")
      (test-input relpath "../a/b" "../a/b")
      (test-input relpath '("a" "../b") (concat "../" curdir "/a"))
      (test-input relpath '("a/b" "../c") (concat "../" curdir "/a/b"))
      (test-input relpath '("a" "b/c") "../../a")
      (test-input relpath '("a" "a") ".")
      (test-input relpath '("/foo/bar/bat" "/x/y/z") "../../../foo/bar/bat")
      (test-input relpath '("/foo/bar/bat" "/foo/bar") "bat")
      (test-input relpath '("/foo/bar/bat" "/") "foo/bar/bat")
      (test-input relpath '("/" "/foo/bar/bat") "../../..")
      (test-input relpath '("/foo/bar/bat" "/x") "../foo/bar/bat")
      (test-input relpath '("/x" "/foo/bar/bat") "../../../x")
      (test-input relpath '("/" "/") ".")
      (test-input relpath '("/a" "/a") ".")
      (test-input relpath '("/a/b" "/a/b") "."))))


(finalize)
