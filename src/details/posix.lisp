(defpackage py.path.details.posix
  (:use :cl :alexandria)
  (:export abspath
           basename
           exists
           expanduser
           expandvars
           isabs
           isdir
           islink
           ismount
           join
           lexists
           normcase
           normpath
           realpath
           relpath
           split
           splitdrive
           splitunc)
  (:shadowing-import-from py.path.details.generic concat string-type getenv))

(in-package py.path.details.posix)

(defmacro osicat-check-no-file (&body body)
  `(handler-case
      (progn ,@body)
    (osicat-posix:enoent (e) nil)))


(defun join (path &rest paths)
  (cond ((null paths) ;; finalizing recursion clause
         path)
        ((starts-with #\/ (car paths))
         ;; a component is an absolute path, discard
         ;; all previous components to the left
         (apply #'join (car paths) (cdr paths)))
        ((or (emptyp path) (ends-with #\/ path))
         ;; just concat and continue to join
         (apply #'join (concat path (car paths)) (cdr paths)))
        (t ;; have to add "/" in between
         (apply #'join (concat path "/" (car paths)) (cdr paths)))))


(defun split-components (path)
  "Splits the path to the list of elements using
slash as a separator. Separators are not omitted.
Example:
  (split-components \"/abc/def/gh//12\")
  => (\"/\" \"abc\" \"/\" \"def\" \"/\" \"gh\" \"//\" \"12\")"
  (unless (emptyp path)
    (let (components)
      (loop with is-sep = (char= #\/ (char path 0))
            with current-word = nil
            for x across path
            for c = (char= #\/ x)
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
          ((ends-with #\/ (last-elt components)) ;; is a directory
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
       (char= (char path 0) #\/)))


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


(defun ismount (path)
  "Determine if the PATH is mount point"
  ;; needed realpath to implement
  )


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


(defun expanduser (path)
  "Expand ~ and ~user inside the PATH.
Return PATH unchanged if unable to expand"
  ;; needed access to environment variables,
  path)


(defun expandvars (path)
  "Expand environment variables in PATH of form $VAR and ${VAR},
if variables exist. Otherwise they stay unchanged in the output."
  path)


(defun normpath(path)
  "Normalize path, removing unnecessary/redundant parts, like dots,
double slashes, etc. Expanding .. as well.
Example:
///..//./foo/.//bar => /foo/bar"
  path)


(defun abspath (path)
  "Return an absolute path."
  path)


(defun realpath(filename)
  "Return real path of the file, following symlinks if necessary"
  filename)


(defun relpath (path &optional (start "."))
  "Convert PATH from absolute to relative"
  path)


  



