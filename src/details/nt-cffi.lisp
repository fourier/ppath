(defpackage py.path.details.nt.cffi
  (:use :cl :cffi)
  (:export get-full-path-name getpid get-temp-path get-file-attributes))

(in-package py.path.details.nt.cffi)

(cffi:define-foreign-library kernel32
  (:win32 "Kernel32"))

(cffi:use-foreign-library kernel32)

(defconstant +win32-encoding+
  #+little-endian :utf-16le
  #+big-endian :utf-16be
  "Windows UTF-16 encoding to be used in CFFI:FOREIGN-STRING-TO-LISP, since Windows
by default uses little-endian order by default")

(defconstant +win32-max-path+ 4096
  "Windowsd MAX_PATH constant. Max path length in letters. Use increased size")

(defcfun ("GetFullPathNameW" get-full-path-name-w) :int32
  (lp-file-name :pointer)
  (n-buffer-length :int32)
  (lp-buffer :pointer)
  (lp-file-part :pointer))

(defun get-full-path-name (filename)
  "On Windows retrieves the full path and file name of the specified file.
Used to convert relative path to absolute."
  (with-foreign-string (lp-file-name filename :encoding +win32-encoding+ :null-terminated-p t)
    (with-foreign-pointer (lp-buffer (* 2 +win32-max-path+))
      (let ((strlen
             (get-full-path-name-w
              lp-file-name
              +win32-max-path+
              lp-buffer
              (null-pointer))))
        (if (> strlen 0)
            (multiple-value-bind (string length)
                (foreign-string-to-lisp lp-buffer :encoding +win32-encoding+)
              (declare (ignore length))
              string)
            nil)))))


(defcfun ("GetTempPathW" get-temp-path-w) :int32
  (n-buffer-length :int32)
  (lp-buffer :pointer))

(defun get-temp-path ()
  "On Windows retrieves the path of the directory where the temporary files reside."
  (with-foreign-pointer (lp-buffer (* 2 +win32-max-path+))
    (let ((strlen
           (get-temp-path-w +win32-max-path+ lp-buffer)))
      (if (> strlen 0)
          (multiple-value-bind (string length)
              (foreign-string-to-lisp lp-buffer :encoding +win32-encoding+)
            (declare (ignore length))
            string)
          nil))))


(defcfun ("GetFileAttributesW" get-file-attributes-w) :int32
  (lp-file-name :pointer))

(defun get-file-attributes (path)
  "Returns the file attributes as an integer.
See https://msdn.microsoft.com/en-us/library/windows/desktop/gg258117(v=vs.85).aspx
for constants list"
  (with-foreign-string (lp-file-name path :encoding +win32-encoding+ :null-terminated-p t)
    (get-file-attributes-w lp-file-name)))
  

(cffi:defcfun ("GetCurrentProcessId" getpid) :int32)
