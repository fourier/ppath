;;;; posix-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :pypath)' in your Lisp.
;;

(in-package :cl-user)
(defpackage py.path.test.posix-test
  (:use :cl
        :alexandria
        :pypath.test.base
        :prove)
   (:shadowing-import-from :py.path.details.posix
    join
    split-components ;; helper function
    split
    isabs
    basename
    dirname
    islink
    sameopenfile
    samefile
    exists
    lexists))

(in-package :py.path.test.posix-test)

;; NOTE: To run this test file, execute `(asdf:test-system :pypath)' in your Lisp.

(plan nil)


(subtest "Test join"
  (test-input join '("/foo" "bar" "/bar" "baz") "/bar/baz")
  (test-input join '("/foo" "bar" "baz") "/foo/bar/baz")
  (test-input join '("/foo/" "bar/" "baz/") "/foo/bar/baz/")
  (test-input join '("/" "/") "/")
  (test-input join '("a" "/bb") "/bb"))

(subtest "Test split-path-components helper function"
  (test-input split-components "/abc/def/gh//12" '("/" "abc" "/" "def" "/" "gh" "//" "12"))
  (test-input split-components "/" '("/"))
  (test-input split-components "/a" '("/" "a"))
  (test-input split-components "a//bcd" '("a" "//" "bcd"))
  (test-input split-components "//a/bcd" '("//" "a" "/" "bcd"))
  (test-input split-components "//a/bcd/" '("//" "a" "/" "bcd" "/"))
  (test-input split-components "" nil))
  

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
  (osicat-posix:mktemp (join (py.path.details.generic:get-temp-path) "test-pypath-XXXXXX")))

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


         

(finalize)
