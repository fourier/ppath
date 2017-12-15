(defpackage py.path.details.posix
  (:use :cl :alexandria)
  (:export abspath
           basename
           expanduser
           expandvars
           isabs
           isdir
           islink
           ismount
           join
           normcase
           normpath
           realpath
           relpath
           split
           splitdrive
           splitunc)
  (:shadowing-import-from py.path.details.generic concat string-type))

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
            finally do (push current-word components))
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


(defun lexists (path)
  "Return true if path exists, regardless if its a link or a file/directory"
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
  (samestat (osicat-posix:stat path1)
            (osicat-posix:stat path2)))


(defun fd-from-stream (stream)
  #+ecl (EXT:FILE-STREAM-FD stream)
  #+sbcl (sb-sys:fd-stream-fd stream)
  #+lispworks (slot-value stream 'stream::file-handle)
  #+ccl (ccl::stream-device stream :input))

(defun sameopenfile (stream1 stream2)
  (samestat (osicat-posix:fstat (fd-from-stream stream1))
            (osicat-posix:stat (fd-from-stream stream2))))
