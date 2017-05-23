(defpackage py.path.details.generic
  (:use :cl :alexandria)
  (:export getenv concat))

(in-package py.path.details.generic)

(declaim (notinline getenv))
(defun getenv (name)
  "Get system environment variable value."
  ;;  (osicat:environment-variable name))
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr xor) ""))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   #+ccl (ccl:getenv name)))


(defun concat (str1 &rest strs)
  "Concatenate strings in a portable manner, converting to unicode string if necessary"
  (let ((str-type
         #+LISPWORKS 
          (let ((lw-strtype #+LISPWORKS7 'lw:simple-bmp-string #+lispworks6 'lw:simple-text-string))
            (if (some (lambda (x) (subtypep (type-of x) lw-strtype)) (cons str1 strs))
                lw-strtype
                'string))
          #-LISPWORKS 'string))
    (apply #'concatenate str-type (cons str1 (mapcar #'string strs)))))
