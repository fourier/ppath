(defpackage py.path.details.posix.cffi
  (:use :cl :cffi)
  (:export getpid))

(in-package py.path.details.posix.cffi)

(cffi:defcfun ("getpid" getpid) :int)
