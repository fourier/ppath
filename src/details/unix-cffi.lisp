(defpackage py.path.details.unix.cffi
  (:use :cl :cffi)
  (:export getpid))

(in-package py.path.details.unix.cffi)

(cffi:defcfun ("getpid" getpid) :int)
