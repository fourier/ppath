#|
  This file is a part of pypath project.
  Copyright (c) 2017 Alexey Veretennikov (alexey.veretennikov@gmail.com)
|#

#|
  File paths library based on Python's os.path
  Author: Alexey Veretennikov (alexey.veretennikov@gmail.com)
|#

(in-package :cl-user)
(defpackage py.path-asd
  (:use :cl :asdf))
(in-package :py.path-asd)

(defsystem #:pypath
  :version "0.1"
  :author "Alexey Veretennikov"
  :license "BSD" ;; https://opensource.org/licenses/bsd-license.php
  :depends-on (#:alexandria     ; general utilities - Public domain
               #:cl-fad         ; files manipulation - BSD
               #:cl-ppcre       ; portable regular expressions - BSD
               #:babel          ; bytes to string - MIT
               #:split-sequence ; general split - public domain
               #:cffi           ; to access dlls (libz) - MIT
               #:static-vectors) ; to use common arrays between C and Lisp code - MIT
  :components ((:module "src"
						:components
						((:module "details"
								  :serial t
								  :components
								  ((:file "generic")
								   (:file "nt")
								   (:file "posix")))
						 (:file "py-path"))))
  :description "A Common Lisp implementation of the Python's os.path module"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op pypath-test))))
