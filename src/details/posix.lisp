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
        
