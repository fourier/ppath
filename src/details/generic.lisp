(defpackage py.path.details.generic
  (:use :cl :alexandria)
  (:export path-error getenv getcwd concat getpid get-temp-path
           commonprefix splitext))

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
  #+windows (py.path.details.nt.cffi:getpid)
  #-windows (py.path.details.posix.cffi:getpid))

(defun get-temp-path ()
  "Return the path to the temporary files directory"
  #+windows (py.path.details.nt.cffi:get-temp-path)
  #-windows "/tmp/")
  
(defun commonprefix (&rest  paths)
  "Get the common prefix substring  of all strings in PATHS"
  (unless paths (return-from commonprefix ""))
  (reduce (lambda (x y)
            (subseq x 0 (or (mismatch x y) (length x)))) paths))


(defun splitext (path)
  "Split path to path and extension. Extension is the text
after the last dot.
Invariant: (concatenate 'string root ext) == p)"
  (flet ((sep-p (c)
           (declare (type  character c))
           (declare (optimize (speed 3) (safety 0)))
           #+windows (or (char= c #\\) (char= c #\/))
           #-windows (char= c #\/)
           ))
    (let ((ext-pos (position #\. path :from-end t))
          (sep-pos (position-if #'sep-p path :from-end t))
          (len (length path)))
      ;; different cases:
      ;; - if the path starts with the dot, keep it as a name
      (cond ((and ext-pos
                  (every (lambda (c) (declare (type character c)) (char= c #\.))
                         (subseq path 0 ext-pos)))
             (cons path ""))
            ;; - if we encountered / before dot, then no extension
            ((and ext-pos sep-pos (> sep-pos ext-pos))
             (cons path ""))
            ;; - if the path ends with dots, no extension
            ((and ext-pos (= ext-pos (1- len)))
             (cons path ""))
            (t
             (let ((ext-pos (if ext-pos ext-pos len))
                   (sep-pos (if sep-pos sep-pos 0)))
               (cons (subseq path 0 (max ext-pos sep-pos))
                     (subseq path (max ext-pos sep-pos) len))))))))

