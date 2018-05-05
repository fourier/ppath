(defpackage ppath.details.generic
  (:use :cl :alexandria)
  (:export path-error string-type getenv getcwd concat getpid get-temp-path
           commonprefix splitext))

(in-package ppath.details.generic)

(define-condition path-error
    (error)
  ((function :initarg :function
             :initform 'unknown
             :reader path-error-function)
   (reason :initarg :reason
           :reader reason))
  (:report (lambda (condition stream)
             (format stream "Path processing: ~a" (reason condition)))))

(deftype string-type ()
         #+lispworks7 
         'lw:simple-bmp-string
         #+lispworks6 'lw:simple-text-string
         #-lispworks 'string)

(declaim (notinline getenv))
(defun getenv (name)
  "Get system environment variable value."
  ;; The function is a wrapper around uiop:getenv declared notinline so
  ;; the tests could override it
  (uiop:getenv name))

(declaim (notinline getcwd))
(defun getcwd ()
  "Get the current working directory as a string"
  ;; Using uiop:getcwd.
  ;; The function is a wrapper around uiop:getcwd declared notinline so
  ;; the tests could override it
  (namestring (uiop:getcwd)))

(defun concat (&rest strs)
  "Concatenate strings in a portable manner, converting to unicode string if necessary"
  (let ((str-type
         #+lispworks 
          (let ((lw-strtype 'string-type))
            (if (some (lambda (x) (subtypep (type-of x) lw-strtype)) strs)
                lw-strtype
                'string))
          #-lispworks 'string))
    (apply #'concatenate str-type (mapcar #'string strs))))


(defun getpid ()
  "Return the current process id"
  #+windows (ppath.details.nt.cffi:getpid)
  #-windows (ppath.details.posix.cffi:getpid))

(defun get-temp-path ()
  "Return the path to the temporary files directory"
  #+windows (ppath.details.nt.cffi:get-temp-path)
  #-windows "/tmp/")
  
(defun commonprefix (&rest paths)
  "Get the common prefix substring  of all strings in PATHS.
PATHS components could also be lists of strings, like results of
SPLIT operation on paths.
If no common prefix return empty string."
  (unless paths (return-from commonprefix ""))
  (reduce (lambda (x y)
            (subseq x 0 (or (mismatch x y :test #'equal) (length x)))) paths))


(defun splitext (path)
  "Split path to path and extension. Extension is the text
after the last dot.
Invariant: (concatenate 'string root ext) == p)"
  (declaim (inline sep-p))
  (flet ((sep-p (c)
           (declare (type  character c))
           (declare (optimize (speed 3) (safety 0)))
           #+windows (or (char= c #\\) (char= c #\/))
           #-windows (char= c #\/)
           ))
    (let ((ext-pos (or (position #\. path :from-end t) -1))
          (sep-pos (or (position-if #'sep-p path :from-end t) -1)))
      (if (>= sep-pos ext-pos) ; encountered slash from right
          (cons path "")       ; return whole path
          ;; check if between slash and dot exist other letters,
          ;; otherwise its a full path like ~/.bashrc
          (loop with i = (1+ sep-pos)
                while (< i ext-pos)
                unless (char= (char path i) #\.) do
               (return (cons (subseq path 0 ext-pos) (subseq path ext-pos)))
                end
                do (incf i)
                finally (return (cons path "")))))))


(defun wildcard-to-regex (pattern &key (case-sensitive-p t) (beginning-of-string t) (end-of-string t))
  "Convert file wildcards to regular expressions. By default the regular
expression is case sensitive. This is regulated by keyword argument
CASE-SENSITIVE-P.
Parameters BEGINNING-OF-STRING and END-OF-STRING identify whether the beginning of the string (^)
or end of string ($) marker should be present.
Supported patters:

 * - everything
 ? - any character
 [range] - any character in range
 [!range] - any character not in range

Example:
=> (wildcard-to-regex \"Photo*.jpg\") 
\"^Photo.*\\\\.jpg$\"
=> (wildcard-to-regex \"Photo*.jpg\" :case-sensitive-p nil) 
\"(?i)^Photo.*\\\\.jpg$\""  
  (let ((regex
         (loop for i below (length pattern)
               for c = (char pattern i)
               if (char= c #\*) ; process * mask
               collect ".*" into result
               else if (char= c #\?) ; process ?
               collect "." into result
               else if (char= c #\[) ; range found
               collect ;;(extract-range i)
               (if-let (close-pos (position #\] pattern :start i)) ;; find closing ]
                   ;; found, replace \ with \\
                   (let ((res (ppcre:regex-replace-all "\\" (subseq pattern (1+ i ) close-pos) "\\\\" )))
                     (setf i close-pos) ; increase current position to the end of range
                     (format nil "[~a]"
                             (cond ((char= (char res 0) #\!)
                                    (concatenate 'string "^" (subseq res 1)))
                                   ((char= (char res 0) #\^)
                                    (concatenate 'string "\\" res))
                                   (t res))))
                 ;; no closing range character found, assuming special
                 "\\[")
               into result
               else ; finally just append rest (quoting specials of course)
               collect (ppcre:quote-meta-chars (string c)) into result
               end
               finally
               (return (apply #'concatenate 'string result)))))
    (concatenate 'string
                 (unless case-sensitive-p "(?i)")
                 (when beginning-of-string "^")
                 regex
                 (when end-of-string "$"))))
