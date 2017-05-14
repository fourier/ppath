(defpackage py.path.details.nt
  (:use :cl :alexandria)
  (:export splitdrive
           split
           splitunc))

(in-package py.path.details.nt)

(define-constant +separator+  #\\)
(define-constant +posix-separator+ #\/)
(define-constant +unc-prefix+ "//" :test #'equal)
(define-constant +path-separator+ #\;)


(defun posixify (path)
  "Replaces '\\' with '/' in path"
  (substitute +posix-separator+ +separator+ path))

(defun splitdrive (path)
  "Split a path to the drive (with letter) and path after drive.
Example:
  (splitdrive \"C:\\Sources\\lisp\")
  => (\"C:\" \"\\Sources\\lisp\")"
  (let ((norm (posixify path)))
    (cond ((and (> (length path) 1)     
                (char= (char norm 1) #\:)) ; paths starting with "X:"
           (cons (subseq path 0 2)
                 (subseq path 2)))
          ((starts-with-subseq +unc-prefix+ norm) ; paths starting with "//"
           (if-let ((pos (position +posix-separator+ norm :start 2)))
               (if-let ((pos1 (position +posix-separator+ norm :start (1+ pos))))
                 (cons (subseq path 0 pos1)
                       (subseq path pos1))
                 (cons "" path))
             (cons "" path)))
          (t (cons "" path)))))


(defun split (path)
  "Split a path to the pair (head tail) where head is everything before
the last '/' and tail is the last '/'"
  (let* ((split (splitdrive path))
         (drive (car split))
         (p     (cdr split))
         (last-slash-pos (position +posix-separator+ (posixify p) :from-end t)))
    (if last-slash-pos 
        (cons (concatenate 'string drive (subseq p 0 last-slash-pos))
              (subseq p (1+ last-slash-pos)))
        (cons "" path))))



(defun splitunc(path)
  "Split a pathname with UNC path. UNC syntax:
\\host-name\share-name\file_path
Return a cons pair (host-name\share-name . file_path)"
  )
  
