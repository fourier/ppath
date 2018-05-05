#|
  This file is a part of ppath project.
  Copyright (c) 2017-2018 Alexey Veretennikov (alexey.veretennikov@gmail.com)
|#

#|
  File paths library based on Python's os.path
  Author: Alexey Veretennikov (alexey.veretennikov@gmail.com)
|#

(in-package :cl-user)


(defpackage ppath-asd
  (:use :cl :asdf))
(in-package :ppath-asd)

(defsystem #:ppath
  :version "0.1"
  :author "Alexey Veretennikov"
  :license "BSD" ;; https://opensource.org/licenses/bsd-license.php
  :depends-on (#:alexandria            ; general utilities - Public domain
               #:cffi                  ; to access dlls (kernel32) - MIT
               #:osicat                ; for sys/stat - MIT
               #:uiop                  ; os operations like getcwd - MIT
               #:trivial-features      ; consistent *features* - MIT
               #:cl-ppcre
               #:split-sequence)       ; general split - public domain
  :components ((:module "src"
                :serial t
                :components
                ((:module "details"
                  :serial t
                  :components
                  ((:file "constants")
                   #+(or windows win32 os-windows) (:file "nt-cffi")
                   #-(or windows win32 os-windows) (:file "posix-cffi")
                   (:file "generic")
                   #+(or windows win32 os-windows) (:file "nt")
                   #-(or windows win32 os-windows) (:file "posix")))
                 (:file "ppath"))))
  :description "A Common Lisp path handling library based on Python's os.path module"
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
  :in-order-to ((test-op (test-op ppath-test))))
