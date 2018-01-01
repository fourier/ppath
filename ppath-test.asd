#|
  This file is a part of pypath project.
  Copyright (c) 2017 Alexey Veretennikov (alexey.veretennikov@gmail.com)

  Test package.
  Usage:
  (ql:quickload :pypath-test)
  (asdf/operate:test-system :pypath)
|#

(in-package :cl-user)
(defpackage pypath-test-asd
  (:use :cl :asdf))

(in-package :pypath-test-asd)

(defsystem pypath-test
  :author "Alexey Veretennikov"
  :license "BSD"
  :description "Test system for pypath"
  :depends-on (:pypath
               :alexandria
               :cl-fad
               :prove
               :prove-asdf)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "t"
                :components
                ((:file "base")
                 #+(or windows win32 os-windows) (:test-file "nt-test")
                 #-(or windows win32 os-windows) (:test-file "posix-test")
                 (:test-file "generic-test"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))

