(defpackage ppath.details.posix.cffi
  (:use :cl :cffi :alexandria)
  (:export getpid))

(in-package ppath.details.posix.cffi)

(defun getpid()
  (osicat-posix:getpid))


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
