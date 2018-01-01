(defpackage ppath.details.posix.cffi
  (:use :cl :cffi :alexandria)
  (:export getpid realpath))

(in-package ppath.details.posix.cffi)

(define-constant +path-max+ 4096)


(defun getpid()
  (osicat-posix:getpid))


;; char * realpath(const char *restrict file_name, char *restrict resolved_name)
(defcfun ("realpath" crealpath) :pointer
  (path :string)
  (resloved-name :pointer))

(defun realpath (path)
  "Wrapper around realpath function from <stdlib.h>"
  (with-foreign-pointer (resolved-name +path-max+)
    (let ((result (crealpath path resolved-name)))
      (when (not (null-pointer-p result))
        (multiple-value-bind (string length)
            (foreign-string-to-lisp resolved-name)
          (declare (ignore length))
          string)))))

;;;;;;;;;;;;;;;;; auxulary functions, needed for debugging

(defun array-to-hex (arr)
  ;; a SLOW version used only for dumping output to logs etc
  (string-downcase
   (with-output-to-string (s)  
     (loop for i below (length arr)
           do
           (format s "~2,'0x" (aref arr i)))
    s)))

(defun foreign-ptr-to-string (ptr length)
  (array-to-hex (coerce (loop for i below length
                              for x = (mem-aref ptr :uint8 i)
                              collect x) 'vector)))
