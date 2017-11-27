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
    split))

(in-package :py.path.test.posix-test)

;; NOTE: To run this test file, execute `(asdf:test-system :pypath)' in your Lisp.

(plan nil)


(subtest "Test join"
  (test-input join '("/foo" "bar" "/bar" "baz") "/bar/baz")
  (test-input join '("/foo" "bar" "baz") "/foo/bar/baz")
  (test-input join '("/foo/" "bar/" "baz/") "/foo/bar/baz/")
  (test-input join '("/" "/") "/")
  (test-input join '("a" "/bb") "/bb"))

(subtest "Test split"
  (test-input split "/foo/bar" '("/foo" "bar"))
  (test-input split "/"  '("/" ""))
  (test-input split "foo" '("" "foo"))
  (test-input split "////foo" '("////" "foo"))
  (test-input split "//foo//bar" '("//foo" "bar")))

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


         

(finalize)
