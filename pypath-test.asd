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
  :depends-on (:pypath
               :alexandria
               :cl-fad
               :prove)
  :components ((:module "t"
                :components
                ((:file "base")
				 (:test-file "nt-test"))))
  :description "Test system for pypath"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))

