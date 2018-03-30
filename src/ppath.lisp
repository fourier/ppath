(defpackage ppath
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
   getsize
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
   #-windows samefile
   #-windows sameopenfile
   split
   splitdrive
   splitext
   #+windows splitunc))


(in-package ppath)
;;(proclaim '(optimize (safety 0) (speed 3)))
(proclaim '(optimize (safety 3) (speed 0) (debug 3)))


(defun abspath (path)
  "Convert relative PATH to absolute. If path is absolute return it unchanged,
if path is empty return current directory

On POSIX systems invariant:
(abspath path) == (normpath (join (getcwd) path))
holds"
  #+windows (ppath.details.nt:abspath path)
  #-windows (ppath.details.posix:abspath path))


(defun basename (path)
  "Extract the base name (filename) of the PATH.

Examples:
On Windows:
CL-USER > (basename \"C:\\\\dir\\\\file.txt\")
=> file.txt

On POSIX:
CL-USER > (basename \"/foo/bar\")
=> bar

Invariant: (basename path) == (cdr (split path))"
  #+windows (ppath.details.nt:basename path)
  #-windows (ppath.details.posix:basename path))


(defun commonprefix (&rest paths)
  "Get the common prefix substring  of all strings in PATHS. The separators are not translated,
so paths interpreted just as normal strings.
PATHS components could also be lists of strings, like results of
SPLIT operation on paths. In this case the comparison happens elementwise.

Example:
CL-USER > (commonprefix '(\"/home/username/dir\" \"/home/user/test\"))
=> /home/user"
  (apply #'ppath.details.generic:commonprefix paths))


(defun dirname (path)
  "Get the directory name of the PATH. If the pt

Example:
CL-USER > (dirname \"/foo/bar\")
=> /foo

Example (Windows):
CL-USER > (dirname \"C:\\\\dir\\\\file.txt\")
=>  C:\\\\dir

Invariant: (dirname path) == (car (split path))"
  #+windows (ppath.details.nt:basename path)
  #-windows (ppath.details.posix:basename path))


(defun exists (path)
  "Check if the PATH is an existing path.
On POSIX returns nil for broken symbolic links."
  #+windows (ppath.details.nt:exists path)
  #-windows (ppath.details.posix:exists path))
  

(defun lexists (path)
  "Check if the PATH is an existing path.
Checks for existance regardless if PATH is a link(even broken) or a file/directory.
On Windows exists=lexists."
  #+windows (ppath.details.nt:exists path)
  #-windows (ppath.details.posix:lexists path))


(defun expanduser (path)
  "Expand ~ and ~user in the PATH with the contents of user's home path.
~ - home directory
~user - user's home directory
Return PATH unchanged if unable to expand.

On Windows the path is taken either from HOME or USERPROFILE, or constructed via HOMEPATH and HOMEDRIVE.
On error just return original PATH value.
On POSIX systems the ~ is replaced with contents of the HOME environment variable or taken from password database
(/etc/passwd or similar).

Examples (POSIX): (given the user \"username\" with home directory /Users/username)
CL-USER > (expanduser \"~/dir\")
=> /Users/username/dir
CL-USER > (expanduser \"~root/dir\")
=> /root/dir

Windows: if HOMEPATH is \"users\\dir\" and HOMEDRIVE is \"C:\\\\\",
CL-USER > (expanduser \"~test\")
=> C:\\users\\test"
  #+windows (ppath.details.nt:expanduser path)
  #-windows (ppath.details.posix:expanduser path))

  
(defun expandvars (path &optional (modify-in-quotes #+windows nil #-windows t))
  "Expand the PATH replacing environment variables with their contents.
The variables like ${var} and $var (and additionally %var% on Windows) are getting replaced by their values.
All unknown or malformed variables ignored and kept as it is.

The difference between Windows and Posix systems is that on Windows variables inside single quotes are not
expanded, i.e. \"'$HOME'\" will remain \"'$HOME'\", while on Posix systems it will be expanded. The optional argument MODIFY-IN-QUOTES allows to change this behavior.
This behavior kept for compatibility with Python's os.path.expandvars.

Examples:
CL-USER > (expandvars \"$HOME/.bashrc\")
=> /home/fourier/.bashrc

CL-USER > (osicat-posix:setenv \"foo\" \"abcd\")
=> 0
CL-USER > (expandvars \"'$foo'$bar\" nil)
=> '$foo'$bar
CL-USER > (expandvars \"'$foo'$bar\" t)
=> 'abcd'$bar"
  #+windows (ppath.details.nt:expandvars path modify-in-quotes)
  #-windows (ppath.details.posix:expandvars path modify-in-quotes))
    

(defun getatime (path)
  "Return the last access time for the PATH.
Return value is seconds since Unix Epoch (1970-01-01T00:00:00Z).
Return nil if unable to access file or get its attributes."
  #+windows (ppath.details.nt:getatime path)
  #-windows (ppath.details.posix:getatime path))


(defun getmtime (path)
  "Return the last modification time for the PATH.
Return value is seconds since Unix Epoch (1970-01-01T00:00:00Z).
Return nil if unable to access file or get its attributes."
  #+windows (ppath.details.nt:getmtime path)
  #-windows (ppath.details.posix:getmtime path))


(defun getctime (path)
  "Return the last status change time for the PATH.
Return value is seconds since Unix Epoch (1970-01-01T00:00:00Z).
Return nil if unable to access file or get its attributes."
  #+windows (ppath.details.nt:getctime path)  
  #-windows (ppath.details.posix:getctime path))


(defun getsize (path)
  "Get the file size in bytes of the PATH.
Return nil if unable to access file or get its attributes."
  #+windows (ppath.details.nt:getsize path)
  #-windows (ppath.details.posix:getsize path))


(defun isabs (path)
  "Determine if the PATH is an absolute pathname.
This function never checks for file existance nor address
file system but rather performs string manipulations to
determine if the PATH is an absolute filename.

Examples (POSIX):
CL-USER > (isabs \"/Sources/lisp\")
=> t
CL-USER > (isabs \"my/dir\")
=> nil

Examples (Windows):
CL-USER > isabs \"\\\\\\\\host-name\\\\share-name\\\\\")
=> t"
  #+windows (ppath.details.nt:isabs path)
  #-windows (ppath.details.posix:isabs path))
  

(defun isfile (path)
  "Determine if the PATH exists and a file. Returns also t for symbolic links."
  #+windows (ppath.details.nt:isfile path)
  #-windows (ppath.details.posix:isfile path))


(defun isdir (path)
  "Determine if PATH is an existing directory. If the PATH is symlink then the invariant
(and (islink PATH) (isdir PATH)) holds."
  #+windows (ppath.details.nt:isdir path)
  #-windows (ppath.details.posix:isdir path))

(defun islink (path)
  "Determine if the PATH is symbolic link.
On Windows always return nil."
  #+windows (ppath.details.nt:islink path)
  #-windows (ppath.details.posix:islink path))
  

;; TODO: documentation for functions below
(defun ismount (path)
  "Return t if pathname PATH is a mount point: a point in a file system where a different file system has been mounted. the function checks whether path‘s parent, path/.., is on a different device than path, or whether path/.. and path point to the same i-node on the same device — this should detect mount points for all unix and posix variants."
  #+windows (ppath.details.nt:ismount path)
  #-windows (ppath.details.posix:ismount path))



(defun join(path &rest paths)
  "Join one or more path components intelligently. The return value is the concatenation of PATH and any members of PATHS with exactly one directory separator following each non-empty part except the last, meaning that the result will only end in a separator if the last part is empty. If a component is an absolute path, all previous components are thrown away and joining continues from the absolute path component.

On windows, the drive letter is not reset when an absolute path component (e.g., \"\foo\") is encountered. If a component contains a drive letter, all previous components are thrown away and the drive letter is reset. Note that since there is a current directory for each drive, (join \"c:\" \"foo\") represents a path relative to the current directory on drive c: (c:foo), not c:\foo."
  #+windows (apply #'ppath.details.nt:join path paths)
  #-windows (apply #'ppath.details.posix:join path paths))


(defun normcase (path)
  "Normalize the case of a pathname. On unix and mac os x, this returns the path unchanged; on windows, it lowercases PATH and converts forward slashes to backward slashes."
  #+windows (ppath.details.nt:normcase path)
  #-windows (ppath.details.posix:normcase path))

(defun normpath (path)
  "Normalize path, removing unnecessary/redundant parts, like dots,
double slashes, etc. Expanding .. as well.
Example:
///..//./foo/.//bar => /foo/bar
On Windows it additionally converts forward slashes to backward slashes."
  #+windows (ppath.details.nt:normpath path)
  #-windows (ppath.details.posix:normpath path))


(defun realpath(path)
  "Return the canonical path of the specified filename, eliminating any symbolic links encountered in the path (if they are supported by the operating system)."
  #+windows (ppath.details.nt:realpath path)
  #-windows (ppath.details.posix:realpath path))

(defun relpath (path &optional (start "."))
  "Return a relative filepath to PATH either from the current directory or from an optional START directory. This is a path computation: the filesystem is not accessed to confirm the existence or nature of path or start.

START defaults to current directory '.'"
  #+windows (ppath.details.nt:relpath path start)
  #-windows (ppath.details.posix:relpath path start))


(defun samefile(path1 path2)
  "return true if both pathname arguments refer to the same file or directory (as indicated by device number and i-node number). raise an exception if a os.stat() call on either pathname fails."
  #+windows (error "Not implemented")
  #-windows (ppath.details.posix:samefile path1 path2))

(defun sameopenfile(fp1 fp2)
  "return true if the file descriptors fp1 and fp2 refer to the same file."
  #+windows (error "Not implemented")
  #-windows (ppath.details.posix:sameopenfile fp1 fp2))


(defun split (path)
  "split the pathname path into a pair, (head, tail) where tail is the last pathname component and head is everything leading up to that. the tail part will never contain a slash; if path ends in a slash, tail will be empty. if there is no slash in path, head will be empty. if path is empty, both head and tail are empty. trailing slashes are stripped from head unless it is the root (one or more slashes only). in all cases, join(head, tail) returns a path to the same location as path (but the strings may differ). also see the functions dirname() and basename()."
  #+windows (ppath.details.nt:split path)
  #-windows (ppath.details.posix:split path))
  

(defun splitdrive (path)
  "split the pathname path into a pair (drive, tail) where drive is either a drive specification or the empty string. on systems which do not use drive specifications, drive will always be the empty string. in all cases, drive + tail will be the same as path."
  #+windows (ppath.details.nt:splitdrive path)
  #-windows (cons "" path))

(defun splitext(path)
  "Split PATH to root and extension. Return a pair (root . ext)
Invariant: (concatenate 'string root ext) == path"
  (ppath.details.generic:splitext path))


(defun splitunc (path)
  "split the pathname path into a pair (unc, rest) so that unc is the unc mount point (such as r'\\host\mount'), if present, and rest the rest of the path (such as r'\path\file.ext'). for paths containing drive letters, unc will always be the empty string."
  #+windows (ppath.details.nt:splitunc path)
  #-windows (declare (ignore path))
  #-windows (error "This function is not available on POSIX systems"))



            
