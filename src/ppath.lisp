(defpackage pypath
  (:use :cl :alexandria)
  (:export
   abspath
   basename
   commonprefix
   dirname
   exists
   lexists
   expanduser
   expandvars
   getatime
   getmtime
   getctime
   isabs
   isfile
   isdir
   islink
   ismount
   join
   normcase
   normpath
   realpath
   relpath
   samefile
   sameopenfile
   samestat
   split
   splitdrive
   splitext
   splitunc
   walk))


(in-package pypath)
;;(proclaim '(optimize (safety 0) (speed 3)))
(proclaim '(optimize (safety 3) (speed 0) (debug 3)))


(defun abspath (path)
  "Convert PATH to the absolute path.
Invariant: (abspath path) == (normpath (join (getcwd) path))."
  #+windows (py.path.details.nt:abspath path)
  #-windows (error "Not implemented"))


(defun basename (path)
  "Get the base name (filename) of PATH.
Invariant: (basename path) == (cdr (split path))"
  #+windows (py.path.details.nt:basename path)
  #-windows (py.path.details.posix:basename path))


(defun commonprefix (&rest paths)
  "Get the common prefix substring  of all strings in PATHS. The separators are not translated,
so paths interpreted just as normal strings"
  (apply #'py.path.details.generic:commonprefix paths))


(defun dirname (path)
  "Get the directory name of the PATH.
Invariant: (dirname path) == (car (split path))"
  #+windows (py.path.details.nt:basename path)
  #-windows (py.path.details.posix:basename path))


(defun exists (path)
  "Return True if path refers to an existing path. Returns nil for broken symbolic links."
  #+windows (py.path.details.nt:exists path)
  #-windows (py.path.details.posix:exists path))
  

(defun lexists (path)
  "Return True if path refers to an existing path. Returns True for broken symbolic links. On Windows exists=lexists."
  #+windows (py.path.details.nt:exists path)
  #-windows (py.path.details.posix:lexists path))


(defun expanduser (path)
  "Expand ~ and ~user in the PATH with the contents of user's home path.
~ - home directory
~user - user's home directory
On Windows the path is taken either from HOME or USERPROFILE, or constructed vua HOMEPATH and HOMEDRIVE.
On error just return original PATH value.
On POSIX systems the ~ is replaced with contents of the HOME environment variable or taken from password database
(/etc/passwd or similar).

Examples: (given the user \"username\" with home directory /Users/username)
CL-USER > (expanduser \"~/dir\")
=> /Users/username/dir
CL-USER > (expanduser \"~root/dir\")
=> /root/dir"
  #+windows (py.path.details.nt:expanduser path)
  #-windows (py.path.details.posix:expanduser path))

  
(defun expandvars (path &optional (modify-in-quotes #+windows nil #-windows t))
  "Expand the PATH replacing environment variables with their contents.
The variables like ${var} and $var (and additionally %var% on Windows) are getting replaced by their values.
All unknown or malformed variables ignored and kept as it is.

The difference between Windows and Posix systems is that on Windows variables inside single quotes are not
expanded, i.e. \"'$HOME'\" will remain \"'$HOME'\", while on Posix systems it will be expanded.
This behavior kept for compatibility with Python."
  #+windows (py.path.details.nt:expandvars path modify-in-quotes)
  #-windows (py.path.details.posix:expandvars path modify-in-quotes))
    

(defun getatime (path)
  "Return the time of last access of path. The return value is a number giving the number of seconds since the epoch (see the time module). Raise os.error if the file does not exist or is inaccessible."
  #+windows (py.path.details.nt:getatime path)
  #-windows (error "Not implemented"))


(defun getmtime (path)
  "Return the time of last modification of path. The return value is a number giving the number of seconds since the epoch (see the time module). Raise os.error if the file does not exist or is inaccessible."
  #+windows (py.path.details.nt:getmtime path)
  #-windows (error "Not implemented"))


(defun getctime (path)
  "Return the system’s ctime which, on some systems (like Unix) is the time of the last metadata change, and, on others (like Windows), is the creation time for path. The return value is a number giving the number of seconds since the epoch (see the time module). Raise os.error if the file does not exist or is inaccessible."
  #+windows (py.path.details.nt:getctime path)  
  #-windows (error "Not implemented"))


(defun getsize (path)
  "return the size, in bytes, of path. raise os.error if the file does not exist or is inaccessible."
  #+windows (py.path.details.nt:getsize path)
  #-windows (error "Not implemented"))


(defun isabs (path)
  "Determine if the PATH is an absolute pathname."
  #+windows (py.path.details.nt:isabs path)
  #-windows (py.path.details.posix:isabs path))
  

(defun isfile (path)
  "return true if path is an existing regular file. this follows symbolic links, so both islink() and isfile() can be true for the same path."
  #+windows (py.path.details.nt:isfile path)
  #-windows (error "Not implemented"))


(defun isdir (path)
  "Determine if PATH is an existing directory. If the PATH is symlink then the invariant
(and (islink PATH) (isdir PATH)) holds."
  #+windows (py.path.details.nt:isdir path)
  #-windows (error "Not implemented"))

(defun islink (path)
  "Determine if the PATH is symbolic link(on OS where symbolic links supported, otherwise always return nil)."
  #+windows (py.path.details.nt:islink path)
  #-windows (error "Not implemented"))
  

;; TODO: documentation for functions below
(defun ismount (path)
  "Return t if pathname PATH is a mount point: a point in a file system where a different file system has been mounted. the function checks whether path‘s parent, path/.., is on a different device than path, or whether path/.. and path point to the same i-node on the same device — this should detect mount points for all unix and posix variants."
  #+windows (py.path.details.nt:ismount path)
  #-windows (error "Not implemented"))



(defun join(path &rest paths)
  "Join one or more path components intelligently. The return value is the concatenation of PATH and any members of PATHS with exactly one directory separator following each non-empty part except the last, meaning that the result will only end in a separator if the last part is empty. If a component is an absolute path, all previous components are thrown away and joining continues from the absolute path component.

On windows, the drive letter is not reset when an absolute path component (e.g., \"\foo\") is encountered. If a component contains a drive letter, all previous components are thrown away and the drive letter is reset. Note that since there is a current directory for each drive, (join \"c:\" \"foo\") represents a path relative to the current directory on drive c: (c:foo), not c:\foo."
  #+windows (apply #'py.path.details.nt:join path paths)
  #-windows (apply #'py.path.details.posix:join path paths))


(defun normcase (path)
  "Normalize the case of a pathname. On unix and mac os x, this returns the path unchanged; on windows, it lowercases PATH and converts forward slashes to backward slashes."
  #+windows (py.path.details.nt:normcase path)
  #-windows (py.path.details.posix:normcase path))

(defun normpath (path)
  "Normalize path, removing unnecessary/redundant parts, like dots,
double slashes, etc. Expanding .. as well.
Example:
///..//./foo/.//bar => /foo/bar
On Windows it additionally converts forward slashes to backward slashes."
  #+windows (py.path.details.nt:normpath path)
  #-windows (py.path.details.posix:normpath path))


(defun realpath(path)
  "Return the canonical path of the specified filename, eliminating any symbolic links encountered in the path (if they are supported by the operating system)."
  #+windows (py.path.details.nt:realpath path)
  #-windows (error "Not implemented"))

(defun relpath (path &optional (start "."))
  "Return a relative filepath to PATH either from the current directory or from an optional START directory. This is a path computation: the filesystem is not accessed to confirm the existence or nature of path or start.

START defaults to current directory '.'"
  #+windows (py.path.details.nt:relpath path start)
  #-windows (error "Not implemented"))


(defun samefile(path1 path2)
  "return true if both pathname arguments refer to the same file or directory (as indicated by device number and i-node number). raise an exception if a os.stat() call on either pathname fails."
    (error "Not implemented"))

(defun sameopenfile(fp1 fp2)
  "return true if the file descriptors fp1 and fp2 refer to the same file."
    (error "Not implemented"))


(defun samestat(stat1 stat2)
  "return true if the stat tuples stat1 and stat2 refer to the same file. these structures may have been returned by os.fstat(), os.lstat(), or os.stat(). this function implements the underlying comparison used by samefile() and sameopenfile()."
    (error "Not implemented"))

(defun split (path)
  "split the pathname path into a pair, (head, tail) where tail is the last pathname component and head is everything leading up to that. the tail part will never contain a slash; if path ends in a slash, tail will be empty. if there is no slash in path, head will be empty. if path is empty, both head and tail are empty. trailing slashes are stripped from head unless it is the root (one or more slashes only). in all cases, join(head, tail) returns a path to the same location as path (but the strings may differ). also see the functions dirname() and basename()."
  #+windows (py.path.details.nt:split path)
  #-windows (py.path.details.posix:split path))
  

(defun splitdrive (path)
  "split the pathname path into a pair (drive, tail) where drive is either a drive specification or the empty string. on systems which do not use drive specifications, drive will always be the empty string. in all cases, drive + tail will be the same as path."
  #+windows (py.path.details.nt:splitdrive path)
  #-windows (error "Not implemented"))

(defun splitext(path)
  "Split PATH to root and extension. Return a pair (root . ext)
Invariant: (concatenate 'string root ext) == path"
  (py.path.details.generic:splitext path))

(defun splitunc (path)
  "split the pathname path into a pair (unc, rest) so that unc is the unc mount point (such as r'\\host\mount'), if present, and rest the rest of the path (such as r'\path\file.ext'). for paths containing drive letters, unc will always be the empty string."
  #+windows (py.path.details.nt:splitunc path)
  #-windows (error "Not implemented"))


(defun walk(path visit arg)
  "calls the function visit with arguments (arg, dirname, names) for each directory in the directory tree rooted at path (including path itself, if it is a directory). the argument dirname specifies the visited directory, the argument names lists the files in the directory (gotten from os.listdir(dirname)). the visit function may modify names to influence the set of directories visited below dirname, e.g. to avoid visiting certain parts of the tree. (the object referred to by names must be modified in place, using del or slice assignment.)

    note

    symbolic links to directories are not treated as subdirectories, and that walk() therefore will not visit them. to visit linked directories you must identify them with os.path.islink(file) and os.path.isdir(file), and invoke walk() as necessary."
  (error "Not implemented"))


            
