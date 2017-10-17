(defpackage py.path.details.posix
  (:use :cl :alexandria)
  (:export abspath
           basename
           expanduser
           expandvars
           isabs
           isdir
           islink
           ismount
           join
           normcase
           normpath
           realpath
           relpath
           split
           splitdrive
           splitunc))

(in-package py.path.details.posix)

(defun join (path &rest paths)
  nil)
