(defpackage py.path.details.generic
  (:use :cl :alexandria)
  (:export getenv))

(in-package py.path.details.generic)

(defun getenv (name)
  "Get system environment variable value."
  (osicat:environment-variable name))
