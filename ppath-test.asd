#|
  This file is a part of ppath project.
  Copyright (c) 2017-2018 Alexey Veretennikov (alexey.veretennikov@gmail.com)

  Test package.
  Usage:
  (ql:quickload :ppath-test)
  (asdf/operate:test-system :ppath)
|#

(in-package :cl-user)
(defpackage ppath-test-asd
  (:use :cl :asdf))

(in-package :ppath-test-asd)

(defsystem ppath-test
  :author "Alexey Veretennikov"
  :license "BSD"
  :description "Test system for ppath"
  :depends-on (:ppath
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

