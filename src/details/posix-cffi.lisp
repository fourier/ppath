(defpackage ppath.details.posix.cffi
  (:use :cl :cffi :alexandria :ppath.details.posix.cffi.package)
  (:export getpid realpath))

(in-package ppath.details.posix.cffi)

(define-constant +path-max+ 4096)

(define-c-struct-wrapper (stat-class ppath.details.posix.cffi.package::stat) ())

(defun fstat (name)
  (with-foreign-object (buf '(:struct ppath.details.posix.cffi.package::stat))
    (let ((result 
           (c-fstat name buf)))
      (when (= result 0)
        (make-instance 'stat :pointer buf)))))


(defun stat (name)
  (with-foreign-object (buf '(:struct ppath.details.posix.cffi.package::stat))
    (let ((result 
           (c-stat name buf)))
      (when (= result 0)
        (make-instance 'stat :pointer buf)))))


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
