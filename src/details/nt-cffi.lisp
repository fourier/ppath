(defpackage py.path.details.nt.cffi
  (:use :cl :cffi)
  (:export
   get-full-path-name
   getpid
   get-temp-path
   get-file-attributes
   get-file-attributes-ex
   ;; file attributes struct and accessors
   file-attribute-data
   file-attribute-data-dw-file-attributes
   file-attribute-data-ft-creation-time-low
   file-attribute-data-ft-creation-time-high
   file-attribute-data-ft-last-access-time-low
   file-attribute-data-ft-last-access-time-high
   file-attribute-data-ft-last-write-time-low
   file-attribute-data-ft-last-write-time-high
   file-attribute-data-n-file-size-high
   file-attribute-data-n-file-size-low))

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

(defconstant +win32-error-sharing-violation+ 32)
  


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


;;;;;;;;;;;;;;;;; GetFileAttributes section ;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun ("GetLastError" get-last-error-w) :uint32)

(defcstruct win32-filetime
  (dw-low-date-time :uint32)
  (dw-high-date-time :uint32))

(defcstruct win32-file-attribute-data
  (dw-file-attributes :uint32)
  (ft-creation-time (:struct win32-filetime))
  (ft-last-access-time (:struct win32-filetime))
  (ft-last-write-time (:struct win32-filetime))
  (n-file-size-high  :uint32)
  (n-file-size-low  :uint32))

(cffi:defcfun ("GetFileAttributesExW" get-file-attributes-ex-w) :int32
  (lp-file-name :pointer)
  (f-info-level-id :int32)
  (lpFileInformation :pointer))

(defstruct file-attribute-data
  (dw-file-attributes 0 :type integer)
  (ft-creation-time-low 0 :type integer)
  (ft-creation-time-high 0 :type integer)
  (ft-last-access-time-low 0 :type integer)
  (ft-last-access-time-high 0 :type integer)
  (ft-last-write-time-low 0 :type integer)
  (ft-last-write-time-high 0 :type integer)
  (n-file-size-high 0 :type integer)
  (n-file-size-low 0 :type integer))

(defun make-file-attribute-data-from-win32 (attr)
  (macrolet ((get-high (field)
                   `(foreign-slot-value 
                     (foreign-slot-pointer attr '(:struct win32-file-attribute-data)
                                          ',field)
                    '(:struct win32-filetime) 'dw-high-date-time))
                 (get-low (field)
                   `(foreign-slot-value 
                     (foreign-slot-pointer attr '(:struct win32-file-attribute-data)
                                          ',field)
                    '(:struct win32-filetime) 'dw-low-date-time)))
    (with-foreign-slots ((dw-file-attributes
                                       n-file-size-high
                                       n-file-size-low)
                                      attr (:struct win32-file-attribute-data))
                   (make-file-attribute-data
                    :dw-file-attributes dw-file-attributes
                    :ft-creation-time-low (get-low ft-creation-time)
                    :ft-creation-time-high (get-high ft-creation-time)
                    :ft-last-access-time-low (get-low ft-last-access-time)
                    :ft-last-access-time-high (get-high ft-last-access-time)
                    :ft-last-write-time-low (get-low ft-last-write-time)
                    :ft-last-write-time-high (get-high ft-last-write-time)
                    :n-file-size-high n-file-size-high
                    :n-file-size-low n-file-size-low))))

(defun get-file-attributes-ex (path)
  "Wrapper around GetFileAttributesExW.
The difference between this version and Python's version is what we don't clear
errno in case of errors.
Also we don't fallback to investigate directory since it should do it automatically
according to https://msdn.microsoft.com/en-us/library/windows/desktop/aa364946(v=vs.85).aspx"
  (with-foreign-string (lp-file-name path :encoding +win32-encoding+ :null-terminated-p t)
    (with-foreign-object (attr '(:pointer (:struct win32-file-attribute-data)))
      (let ((attr-result (get-file-attributes-ex-w lp-file-name 0 attr)))
        (unless (= attr-result 0) ; not failed to get attributes
          (make-file-attribute-data-from-win32 attr))))))

               

      
