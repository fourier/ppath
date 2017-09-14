(defpackage py.path.details.generic
  (:use :cl :alexandria)
  (:export path-error getenv getcwd concat getpid get-temp-path
           commonprefix))

(in-package py.path.details.generic)

(define-condition path-error
    (error)
  ((function :initarg :function
             :initform 'unknown
             :reader path-error-function)
   (reason :initarg :reason
           :reader reason))
  (:report (lambda (condition stream)
             (format stream "Path processing: ~a" (reason condition)))))


(declaim (notinline getenv))
(defun getenv (name)
  "Get system environment variable value."
  ;; The function is a wrapper around uiop:getenv declared notinline so
  ;; the tests could override it
  (uiop:getenv name))

(declaim (notinline getcwd))
(defun getcwd ()
  "Get the current working directory as a string"
  ;; Based on uiop:getcwd. 
  (namestring (uiop:getcwd)))

(defun concat (str1 &rest strs)
  "Concatenate strings in a portable manner, converting to unicode string if necessary"
  (let ((str-type
         #+lispworks 
          (let ((lw-strtype #+lispworks7 'lw:simple-bmp-string #+lispworks6 'lw:simple-text-string))
            (if (some (lambda (x) (subtypep (type-of x) lw-strtype)) (cons str1 strs))
                lw-strtype
                'string))
          #-lispworks 'string))
    (apply #'concatenate str-type (cons str1 (mapcar #'string strs)))))


(defun getpid ()
  "Return the current process id"
  #+(or windows win32) (py.path.details.nt.cffi:getpid)
  #-(or windows win32) (py.path.details.nt.unix:getpid))

(defun get-temp-path ()
  "Return the path to the temporary files directory"
  #+(or windows win32) (py.path.details.nt.cffi:get-temp-path)
  #-(or windows win32) "/tmp/")
  
(defun commonprefix (&rest  paths)
  "Get the common prefix substring  of all strings in PATHS"
  (unless paths
    (return-from commonprefix ""))
  (reduce (lambda (x y)
            (subseq x 0 (or (mismatch x y)
                            (length x)))) paths))
