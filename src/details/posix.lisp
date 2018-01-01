(defpackage ppath.details.posix
  (:use :cl :alexandria :ppath.details.constants)
  (:export abspath
           basename
           exists
           expanduser
           expandvars
           getsize
           isabs
           isdir
           isfile
           islink
           ismount
           join
           lexists
           normcase
           normpath
           realpath
           relpath
           samefile
           sameopenfile
           split
           splitdrive
           splitunc)
  (:shadowing-import-from ppath.details.generic
   concat
   string-type
   getenv
   getcwd
   commonprefix))

(in-package ppath.details.posix)

(defmacro osicat-check-no-file (&body body)
  `(handler-case
      (progn ,@body)
    (osicat-posix:enoent (e) nil)))


(defun join (path &rest paths)
  (unless path
    (setf path ""))
  (cond ((null paths) ;; finalizing recursion clause
         path)
        ((starts-with +separator+ (car paths))
         ;; a component is an absolute path, discard
         ;; all previous components to the left
         (apply #'join (car paths) (cdr paths)))
        ((or (emptyp path) (ends-with +separator+ path))
         ;; just concat and continue to join
         (apply #'join (concat path (car paths)) (cdr paths)))
        (t ;; have to add "/" in between
         (apply #'join (concat path +sep-string+ (car paths)) (cdr paths)))))


(defun split-components (path)
  "Splits the path to the list of elements using
slash as a separator. Separators are not omitted.
Example:
  (split-components \"/abc/def/gh//12\")
  => (\"/\" \"abc\" \"/\" \"def\" \"/\" \"gh\" \"//\" \"12\")"
  (unless (emptyp path)
    (let (components)
      (loop with is-sep = (char= +separator+ (char path 0))
            with current-word = nil
            for x across path
            for c = (char= +separator+ x)
            if (eql c is-sep) do
            (push x current-word)
            else do
            (progn
              (push current-word components)
              (setf is-sep c)
              (setf current-word nil)
              (push x current-word))
            end
            finally (push current-word components))
      (nreverse
       (mapcar (compose (rcurry #'coerce 'string-type) #'nreverse) components)))))


(defun split (path)
  "Split the path into the pair (directory . filename).
If the path ends with \"/\", the file component is empty"
  (let ((components (split-components path)))
    (cond ((not components) ;; empty clause
           (cons "" ""))
          ((ends-with +separator+ (last-elt components)) ;; is a directory
           (cons (apply #'concat components) ""))
          (t
           (let ((path-comps (butlast components)))
             (cons (apply #'concat
                          ;; drop the last separator if the path components
                          ;; is bigger than 1
                          (if (> (length path-comps) 1)
                              (butlast path-comps)
                              path-comps)) (last-elt components)))))))


(defun isabs (path)
  "Return t if the path is absolute."
  (and (> (length path) 0)
       (char= (char path 0) +separator+)))


(defun normcase (path)
  "Normalize case of PATH.
On case-sensitive file systems (default on Linux and OSX) just returns PATH unchanged"
  path)


(defun splitdrive (path)
  "Split PATH into drive and path.
Doing nothing on POSIX, drive component will be empty"
  (cons "" path))


(defun basename (path)
  "Returns filename component of the PATH"
  (cdr (split path)))


(defun dirname (path)
  "Returns directory component of the PATH"
  (car (split path)))



(defun islink (path)
  "Determine if the PATH is a symbolic link"
  (osicat-check-no-file
    (let ((stat (osicat-posix:stat path))
          (lstat (osicat-posix:lstat path)))
      (not (samestat stat lstat)))))


(defun exists (path)
  "Return true if path exists. If file is a broken link return nil"
  (osicat-check-no-file 
    (let ((stat (osicat-posix:stat path)))
      (not (null stat)))))


(defun lexists (path)
  "Return true if path exists, regardless if its a link(even broken) or a file/directory"
  (osicat-check-no-file 
    (let ((lstat (osicat-posix:lstat path)))
      (not (null lstat)))))

(defun samestat(st1 st2)
  "Tests if 2 osicat-posix:stat structs are the same"
  (and (equalp (slot-value st1 'osicat-posix::atime)
               (slot-value st2 'osicat-posix::atime))
       (equalp (slot-value st1 'osicat-posix::ctime)
               (slot-value st2 'osicat-posix::ctime))
       (equalp (slot-value st1 'osicat-posix::mtime)
               (slot-value st2 'osicat-posix::mtime))
       (equalp (slot-value st1 'osicat-posix::blocks)
               (slot-value st2 'osicat-posix::blocks))
       (equalp (slot-value st1 'osicat-posix::ino)
               (slot-value st2 'osicat-posix::ino))
       (equalp (slot-value st1 'osicat-posix::ino)
               (slot-value st2 'osicat-posix::ino))
       (equalp (slot-value st1 'osicat-posix::mode)
               (slot-value st2 'osicat-posix::mode))
       (equalp (slot-value st1 'osicat-posix::size)
               (slot-value st2 'osicat-posix::size))))


(defun samefile (path1 path2)
  "Determine if PATH1 and PATH2 are the paths to the same file.
If one of the paths is symlink to another they considered the same."
  (samestat (osicat-posix:stat path1)
            (osicat-posix:stat path2)))


(defun fd-from-stream (stream)
  "Get the posix file handle from open STREAM"
  #+ecl (EXT:FILE-STREAM-FD stream)
  #+sbcl (sb-sys:fd-stream-fd stream)
  #+lispworks (slot-value stream 'stream::file-handle)
  #+ccl (ccl::stream-device stream :input))


(defun sameopenfile (stream1 stream2)
  "Determine if the open file streams STREAM1 and STREAM2 are
of the same file"
  (samestat (osicat-posix:fstat (fd-from-stream stream1))
            (osicat-posix:fstat (fd-from-stream stream2))))


(declaim (notinline getuid))
(defun getuid ()
  "Get the current user id"
  ;; The function is a wrapper around osicat-posix:getuid declared notinline so
  ;; the tests could override it
  (osicat-posix:getuid))


(declaim (notinline getpwuid))
(defun getpwuid (uid)
  "Get the list of values from DirectoryService (and /etc/passwd) by user id.
Returns the list:
username
password(will have only * most likely)
user id
group id
full user name
user home directory
user shell"
  ;; The function is a wrapper around osicat-posix:getpwuid declared notinline so
  ;; the tests could override it
  (multiple-value-list
   (osicat-posix:getpwuid uid)))


(declaim (notinline getpwnam))
(defun getpwnam (username)
  "Get the list of values from DirectoryService (and /etc/passwd) by user name.
Returns the list:
username
password(will have only * most likely)
user id
group id
full user name
user home directory
user shell"
  ;; The function is a wrapper around osicat-posix:getpwnam declared notinline so
  ;; the tests could override it
  (multiple-value-list
   (osicat-posix:getpwnam username)))


(defun or-strings (arg &rest args)
  "Auxulary helper function, return the first not empty string from its arguments"
  (find-if (compose #'not #'emptyp) (cons arg args)))

(defun expanduser (path)
  "Expand ~ and ~user inside the PATH.
If the PATH not starts with ~ it return unchanged.
Return PATH unchanged if unable to expand"
  ;; path shall start with ~
  (unless (starts-with #\~ path)
    (return-from expanduser path))
  ;; position of the first slash in the string
  (let* ((slash (or (position +separator+ path) (length path)))
         ;; home directory without trailing /
         (homedir (string-right-trim +sep-string+
                                     ;; if the path like ~/ then its our user
                                     (if (= slash 1)
                                         ;; get the value from environment variable HOME if possible
                                         (if-let (home (getenv "HOME"))
                                             home
                                           ;; otherwise get the path using uid and getpwuid
                                           (nth 5 (getpwuid (getuid))))
                                         ;; extract the username from path, like ~user/ => user
                                         ;; and get his home directory using getpwnam
                                         (nth 5 (getpwnam (subseq path 1 slash)))))))
    ;; construct the path by concatenating home directory and the rest
    ;; of the string, or just return slash if both are emtpy
    (or-strings (concat homedir (subseq path slash)) +sep-string+)))
               
    

(defun expandvars (path &optional (modify-in-quotes t))
  "Expand environment variables in PATH of form $VAR and ${VAR},
if variables exist. Otherwise they stay unchanged in the output.
Optional argument MODIFY-IN-QUOTES if set to t (default) perform
replacement inside single quotes. Otherwise strings like
'$HOME' will left as it is."
  (declare (optimize (debug 3) (speed 0) (safety 3)))
  (unless (and (not (emptyp path))
               (find #\$ path))
    (return-from expandvars path))
  (loop
   with res = ""
   with i = 0
   with len = (length path)
   with last = (1- len)
   while (< i len)
   for c = (char path i)
   do
   (macrolet ((acc (var &rest vars)
                `(setf res (concat res ,var ,@vars))))
     (case c
       (#\'                         ; single quote
        (if modify-in-quotes ; if perform replacement in quotes just
            (acc c)          ; treat single quote as any other character
            ;; otherwise find next "closing" single quote and move position
            (let ((next-quote (or (position #\' path :start (1+ i)) len)))
              (acc (subseq path i (min (1+ next-quote) len)))
              (setf i next-quote))))
       (#\$
        ;; looking for ${var} constructions
        (if (and (< i last) (char= (char path (1+ i)) #\{))
            (if-let (end-of-var (position #\} path :start (+ 2 i)))
                ;; extract variable from {}
                (let ((var (subseq path (+ 2 i) end-of-var)))
                  (acc (if (getenv var) (getenv var) (concat "$" var)))
                  (setf i  end-of-var))
              ;; no closing "}" found
              (setf res (concat res (subseq path i)) 
                    i last))
            ;; all other cases: $var and alike
            (let* ((end-of-var (or (position-if-not #'alphanumericp path :start (1+ i)) len))
                   (var (subseq path (1+ i) end-of-var)))
              (acc (if (getenv var) (getenv var) (concat "$" var)))
              (setf i (1- end-of-var)))))
       ;; not special character, just accumulate it
       (otherwise (acc c))))
   (incf i)
   finally (return res)))

(defun normpath(path)
  "Normalize path, removing unnecessary/redundant parts, like dots,
double slashes, etc. Expanding .. as well.
Example:
///..//./foo/.//bar => /foo/bar"
  (when (emptyp path)
    (return-from normpath +current-dir+))
  ;; split path to components like "/" "my" "/" "directory"
  (let* ((components (split-components path)))
    ;; process them. 
    ;; First consider tripple and more slashes as a single slash
    (when (starts-with-subseq "///" (car components))
      (setf (car components) +sep-string+))
    (let* ((head (when (starts-with +separator+ (car components)) (car components)))
           ;; next remove slashes and dots
           (tail
            (remove-if (lambda (x)
                         (or (starts-with +separator+ x)
                             (equal +current-dir+ x)))
                       (if head (cdr components) components)))
           (result))
      (cond (tail
             (loop with prev-normal-dir = nil
                   for p in tail
                   do
                   (flet ((isup () (equal p +up-dir+))) ; shortcut
                     (cond ((and (isup) (not prev-normal-dir) head) ; for paths like /../..
                            nil)                                    ; ignore '..' parts
                           ((and prev-normal-dir (isup)) ; if previous directory was normal and 
                            (pop result)                 ; the current dir is '..', pop previous
                            (setf prev-normal-dir nil))
                           (t
                            ;; otherwise push entry to the result list and set the previous
                            ;; normal directory variable so it can be popped if next entry
                            ;; is '..'
                            (push p result)
                            (when (not (isup))
                              (setf prev-normal-dir p))))))
             (cond (result
                    (join head (apply #'join (nreverse result))))
                   ;; if we eliminated everything but have a "/" initial, return it
                   ((and (not result) head)
                    head)
                   ;; if we eliminated everything and didn't have "/" initial,
                   ;; then its the current directory
                    (t +current-dir+)))
            ;; if nothing after initial slashes, return them
            (t head)))))



(defun abspath (path)
  "Return an absolute path."
  (normpath (join (getcwd) path)))
  

(defun realpath(filename)
  "Return real path of the file, following symlinks if necessary"
  ;; requires abspath 
  filename)


(defun relpath (path &optional (start +current-dir+))
  "Convert PATH from absolute to relative"
  ;; sanity check
  (when (emptyp path)
    (return-from relpath nil))
  ;; get the common prefix of absolutized paths
  (let* ((abspath (split-components (abspath path)))
         (absstart (split-components (abspath start)))
         (common (commonprefix abspath absstart)))
    (when (emptyp common)
      (return-from relpath +current-dir+))
    ;; create common string
    (let* ((common-components (count-if-not (curry #'equal +sep-string+) common))
           ;; number of directories we have to go from the start
           ;; to reach the common directory entry
           (dirs-up (- (count-if-not (curry #'equal +sep-string+) absstart)
                       common-components))
           ;; position where the abspath and common path diverges
           (mismatch (mismatch abspath common))
           ;; get the unique path part, or just whole if they match
           (unique (if mismatch (subseq abspath mismatch) common)))
      (cond ((and (not mismatch) (> dirs-up 0))
             (format nil "~{~A~^/~}"
                     (make-list dirs-up :initial-element +up-dir+)))
            ((not mismatch) +current-dir+)
            ((> dirs-up 0)
             (format nil "~{~A~^/~}/~A"
                     (make-list dirs-up :initial-element +up-dir+)
                     (apply #'concat unique)))
            ((starts-with +sep-string+ unique :test #'equal)
             (apply #'concat (cdr unique)))
            (t (apply #'concat unique))))))


(defun ismount (path)
  "Determine if the PATH is a mount point"
  ;; needed realpath to implement
  )
  

(defun isdir (path)
  "Determine if the PATH is a directory"
  (osicat-check-no-file
    (let ((stat (osicat-posix:stat path)))
      (if t t nil))))
      

(defun isfile (path)
  "Determine if the PATH is a regular file"
  (osicat-check-no-file
    (let ((stat (osicat-posix:stat path)))
      (if t t nil))))


(defun getsize (path)
  "Return the file size"
  (osicat-check-no-file
    (let ((stat (osicat-posix:stat path)))
      (slot-value stat 'osicat-posix::size))))
  


