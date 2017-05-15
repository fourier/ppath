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
  "Split a path to the pair (head . tail) where head is everything before
the last '/' and tail is after the last '/'.
The slashes are stripped from the head and tail.
If the head is a drive name, the slashes are not stripped from it."
  (let* ((split (splitdrive path))
         (drive (car split))
         (p     (cdr split))
         (last-slash-pos (position +posix-separator+ (posixify path) :from-end t)))
    (format t "drive ~a path ~a last-slash-pos ~a length ~a~%" drive p last-slash-pos (length path))
    (cond ((and last-slash-pos (< last-slash-pos (1- (length path))))
           (cons (subseq path 0 last-slash-pos)
                 (subseq path (1+ last-slash-pos))))
          ((and drive (or (not p) (string= (posixify p) +posix-separator+)))               ; case when only drive like C:\\ provided
           (cons path ""))
           (t (cons "" path)))))
        


(defun splitunc(path)
  "Split a pathname with UNC path. UNC syntax:
\\host-name\share-name\file_path
Return a cons pair (host-name\share-name . file_path)"
  )
  
