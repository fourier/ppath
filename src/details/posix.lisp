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
           splitunc)
  (:shadowing-import-from py.path.details.generic concat))

(in-package py.path.details.posix)

(defun join (path &rest paths)
  (cond ((null paths) ;; finalizing recursion clause
         path)
        ((starts-with #\/ (car paths))
         ;; a component is an absolute path, discard
         ;; all previous components to the left
         (apply #'join (car paths) (cdr paths)))
        ((or (emptyp path) (ends-with #\/ path))
         ;; just concat and continue to join
         (apply #'join (concat path (car paths)) (cdr paths)))
        (t ;; have to add "/" in between
         (apply #'join (concat path "/" (car paths)) (cdr paths)))))
         

(defun split (path)
  nil)
