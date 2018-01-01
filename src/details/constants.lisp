(defpackage py.path.details.constants
  (:use :cl :alexandria)
  (:export
   +separator+
   +sep-string+
   +posix-separator+
   +unc-prefix+
   +path-separator+
   +current-dir+
   +up-dir+
   +secs-between-epochs+))

(in-package py.path.details.constants)

(define-constant +separator+  #+windows #\\ #-windows #\/)
(define-constant +sep-string+ (string +separator+))
(define-constant +posix-separator+ #\/)
(define-constant +unc-prefix+ "//" :test #'equal)
(define-constant +path-separator+ #+windows #\; #-windows #\:)
(define-constant +current-dir+ "." :test #'equal)
(define-constant +up-dir+ ".." :test #'equal)
(define-constant +secs-between-epochs+ 11644473600
                 :documentation "Seconds between 1.1.1601 and 1.1.1970")
