(defpackage py.path.details.generic
  (:use :cl :alexandria))

(in-package py.path.details.generic)

(defun empty (str)
  "Returns t if the string is empty"
  (or (not str)
      (= (length str) 0)))
