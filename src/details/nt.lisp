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
  => (\"C:\" \"\\Sources\\lisp\")
This function also parses the UNC paths, providing \\\\hostname\\mountpoint
as a drive part."
  (let ((norm (posixify path)))
    (cond ((and (> (length path) 1)     
                (char= (char norm 1) #\:)) ; paths starting with "X:"
           (cons (subseq path 0 2)
                 (subseq path 2)))
          ((starts-with-subseq +unc-prefix+ norm) ; paths starting with "//"
		   ;; find first slash position after the prefix (like \\host\sharename)
           (if-let ((pos (position +posix-separator+ norm :start 2)))
			 ;; if found, find the next slash position (like \\host\sharename\dir)
			 (if-let ((pos1 (position +posix-separator+ norm :start (1+ pos))))
			   ;; extract the path around this last slash
                 (cons (subseq path 0 pos1)
                       (subseq path pos1))
                 (cons path ""))
             (cons "" path)))
          (t (cons "" path)))))


(defun split (path)
  "Split a path to the pair (head . tail) where head is everything before
the last '/' and tail is after the last '/'.
The slashes are stripped from the head and tail.
If the head is a drive name, the slashes are not stripped from it."
  (flet ((sep-p (c) (or (char= c #\\) (char= c #\/))))
	(let* ((split (splitdrive path))
		   (drive (car split))
		   (p     (cdr split))
		   (i (position-if #'sep-p p :from-end t)) ; position of the last slash
		   (ni (if i (1+ i) 0))			   ; position after last slash
		   ;; split to head and tail at the position after the last slash
		   (head (subseq p 0 ni))
		   (tail (subseq p ni))
		   ;; position in the head of first non-separator
		   (j (position-if-not #'sep-p head :from-end t))) 
	  (cons (concatenate 'string
						 drive
						 (if (and j (> j 0)) (subseq head 0 (1+ j)) head))
			tail))))
 

(defun splitunc(path)
  "Split a pathname with UNC path. UNC syntax:
\\\\host-name\\share-name\\file_path
Return a cons pair (\\\\host-name\\share-name . \\file_path)"
  (let ((norm (posixify path))
		pos
		pos1)
	(if (and (starts-with-subseq +unc-prefix+ norm) ;; paths starting with "//"
			 (setq pos (position +posix-separator+ norm :start 2)) ;; second '/' exists
			 (/= pos 2)	;; and it is not 3 in the row at the beginning
			 (setq pos1 (position +posix-separator+ norm :start (1+ pos))) ;; directory component exists
			 (/= (1+ pos) pos1)) ;; and it is not not 2 slashes in the second component
		;; extract the path around this last slash
		(cons (subseq path 0 pos1)
			  (subseq path pos1))
		;; otherwise head is empty but tail is the path given
		(cons "" path))))

