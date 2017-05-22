(defpackage py.path.details.nt
  (:use :cl :alexandria)
  ;; (:shadowing-import-from py.path.details.generic
  ;;                         empty)
  (:export splitdrive
           split
           splitunc
           isabs
           normcase
           basename
           dirname
           islink
           ismount
		   join))

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
  (flet ((sep-p (c) (or (char= c +separator+) (char= c +posix-separator+))))
    (destructuring-bind (drive . p) (splitdrive path)
      (let* ((i (position-if #'sep-p p :from-end t)) ; position of the last slash
             (ni (if i (1+ i) 0))			   ; position after last slash
             ;; split to head and tail at the position after the last slash
             (head (subseq p 0 ni))
             (tail (subseq p ni))
             ;; position in the head of first non-separator
             (j (position-if-not #'sep-p head :from-end t))) 
        (cons (concatenate 'string
                           drive
                           (if (and j (> j 0)) (subseq head 0 (1+ j)) head))
              tail)))))


(defun splitunc (path)
  "Split a pathname with UNC path. UNC syntax:
\\\\host-name\\share-name\\file_path
Return a cons pair (\\\\host-name\\share-name . \\file_path)"
  (let ((norm (posixify path))
		pos
		pos1)
	(when (not (starts-with-subseq +unc-prefix+ norm))
	  ;; paths must start with "//"
	  (return-from splitunc (cons "" path)))
	(unless (setq pos (position +posix-separator+ norm :start 2))
	  ;; second '/' must exists
	  (return-from splitunc (cons "" path)))
	(when (= pos 2)
	  ;; and it is not 3 in the row at the beginning
	  (return-from splitunc (cons "" path)))
	;; if directory component exists
	(cond ((and (setq pos1 (position +posix-separator+ norm :start (1+ pos)))
				(/= (1+ pos) pos1)) ;; and it is not not 2 slashes in the second component
		   ;; extract the path around this last slash
		   (cons (subseq path 0 pos1)
				 (subseq path pos1)))
		  ((not pos1)					; no directory component exist, return unc
		   (cons path ""))
		  (t 
		   ;; otherwise head is empty but tail is the path given
		   (cons "" path)))))


(defun starts-with-slash (path)
  "Return t if the PATH starts with either forward or backward slash"
  (and (> (length path) 0)
	   (let ((c (char path 0)))
		 (or (char= c +separator+) (char= c +posix-separator+)))))

(defun ends-with-slash (path)
  "Return t if the PATH ends with either forward or backward slash"
  (and (> (length path) 0)
	   (let ((c (last-elt path)))
		 (or (char= c +separator+) (char= c +posix-separator+)))))


(defun isabs (path)
  "Return t if the path is absolute."
  (destructuring-bind (drive . p) (splitdrive path)
    (declare (ignore drive))
    (and (not (emptyp p))
		 (starts-with-slash p))))


(defun normcase (path)
  "Replace slash with backslahes and lowers the case of the string"
  (string-downcase (posixify path)))


(defun basename (path)
  "Returns filename portion of path"
  (cdr (split path)))


(defun dirname (path)
  "Returns directory portion of path"
  (car (split path)))


(defun islink (path)
  "Test if the path is a symbolic link. Returs falso on Win32"
  (declare (ignore path))
  nil)


(defun ismount (path)
  "Test if the path is a mount point. For local paths it should be
absolute path, for UNC it should be mount point of the host"
  (destructuring-bind (unc . p) (splitunc path)
    (if (not (emptyp unc))
        (or (emptyp p)
            (string= p "/")
            (string= p "\\"))
        (destructuring-bind (drive . p) (splitdrive path)
          (declare (ignore drive))
          (or (string= p "/")
              (string= p "\\"))))))


(defun join (path &rest paths)
  "Join paths provided, merging (if absolute) and
inserting missing separators.
Examples:
  (join \"a/b\" \"x/y\")
=> \"a/b\\\\x/y\"
  (join \"c:\\\\hello\" \"world/test.txt\")
=> \"c:\\\\hello\\\\world/test.txt\""
  (destructuring-bind (drive1 . p1) (splitdrive path)
	;; do iterations
	;; when drive and path are not empty but path doesn't start wit slash
	(destructuring-bind (drive . p)
		(join-iter drive1 p1 paths)
	  (if (and (not (emptyp p))
			   (not (starts-with-slash p))
			   (not (emptyp drive))
			   (char/= (last-elt drive) #\:))
		  (concatenate 'string drive (string +separator+) p)
		  (concatenate 'string drive p)))))
			 


(defun join-iter (drive1 p1 paths)
  "Helper function for recursive concatenation of paths.
PATHS is a list of rest paths, drive1 and p1 split first path"
  (flet ((samedrive (d1)
		   (string= (normcase d1) (normcase drive1)))
		 (concat-paths (p1 p2)
		   (if (and (> (length p1) 0) (not (ends-with-slash p1)))
			   (concatenate 'string p1 "\\" p2)
			   (concatenate 'string p1 p2))))
	(if (null paths)
		(cons drive1 p1)	; degraded case
		;; Normal case. get drive and path components of the each of paths	  
		(destructuring-bind (drive . p) (splitdrive (car paths))
		  ;; if this path is absolute or the original path is not absolute
		  (cond ((and (starts-with-slash p)
					  (or (not (emptyp drive))
						  (emptyp drive1)))
				 ;; continue iteration taking new drive				 
				 (join-iter drive p (cdr paths)))
				;; otherwise use original drive
				((starts-with-slash p)
				 (join-iter drive1 p (cdr paths)))
				;; different drives, including unc
				((and (not (emptyp drive))
					  (not (string= drive drive1)) ; different drives
					  (not (samedrive drive)))
				 (join-iter drive p (cdr paths)))
				((and (not (emptyp drive))
					  (not (string= drive drive1))) ; different case of drives, use new
				 (join-iter drive (concat-paths p1 p) (cdr paths)))
				(t
				 ;; relative paths, use old drive and concatenate paths as needed
				 (join-iter drive1 (concat-paths p1 p) (cdr paths))))))))

