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
   splitparts
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

Example:
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
  "Get the directory name of the PATH. 

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

Example:
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
  

(defun ismount (path)
  "Test if the path is a mount point.
On POSIX it is a directory where another filesystem is mounted.
On Windows for local paths it should be
an absolute path, for UNC it should be mount point of the host

Example:
CL-USER > (ismount \"/mnt\")
=> nil
CL-USER > (ismount \"/mnt/cdrom\")
=> t"
  #+windows (ppath.details.nt:ismount path)
  #-windows (ppath.details.posix:ismount path))


(defun join(path &rest paths)
  "Join paths provided, merging (if absolute) and
inserting missing separators.

Example:
CL-USER > (join \"a/b\" \"x/y\")
=> a/b\\\\x/y
CL-USER > (join \"c:\\\\hello\" \"world/test.txt\")
=> c:\\\\hello\\\\world/test.txt
CL-USER > (join \"/foo\" \"bar\" \"baz\")
=> /foo/bar/baz
CL-USER > (join \"/foo/\" \"bar/\" \"baz/\")
=> /foo/bar/baz/"
  #+windows (apply #'ppath.details.nt:join path paths)
  #-windows (apply #'ppath.details.posix:join path paths))


(defun normcase (path)
  "Normalize the PATH.
On Windows, replace slash with backslahes and lowers the case of the PATH.
On POSIX do nothing and just return PATH."
  #+windows (ppath.details.nt:normcase path)
  #-windows (ppath.details.posix:normcase path))

(defun normpath (path)
  "Normalize path, removing unnecessary/redundant parts, like dots,
double slashes, etc. Expanding .. as well.

Example:
CL-USER > (normpath \"///..//./foo/.//bar\")
=> /foo/bar"
  #+windows (ppath.details.nt:normpath path)
  #-windows (ppath.details.posix:normpath path))


(defun realpath(path)
  "Return real PATH of the file, following symlinks if necessary.
On Windows just return (abspath path).
The PATH shall be already expanded properly.
Return nil if PATH does not exist or not accessible"
  #+windows (ppath.details.nt:realpath path)
  #-windows (ppath.details.posix:realpath path))


(defun relpath (path &optional (start "."))
  "Return the relative version of the PATH.
If STARTDIR specified, use this as a current directory to resolve against."
  #+windows (ppath.details.nt:relpath path start)
  #-windows (ppath.details.posix:relpath path start))


(defun samefile(path1 path2)
  "Determine if PATH1 and PATH2 are the paths to the same file.
If one of the paths is symlink to another they considered the same.

Not available on Windows."
  #+windows (error "Not implemented")
  #-windows (ppath.details.posix:samefile path1 path2))

(defun sameopenfile(fp1 fp2)
  "Determine if the open file streams STREAM1 and STREAM2 are
of the same file.

Not available on Windows."
  #+windows (error "Not implemented")
  #-windows (ppath.details.posix:sameopenfile fp1 fp2))


(defun split (path)
  "Split the path into the pair (directory . filename).
If the path ends with \"/\", the file component is empty

On Windows, if the head is a drive name, the slashes are not stripped from it.

Examples:
(On Windows)
CL-USER > (split \"c:\\\\Sources\\\\lisp\")
=> (\"c:\\\\Sources\" . \"lisp\")
CL-USER > (split \"\\\\\\\\host-name\\\\share-name\\\\dir1\\\\dir2\")
=> (\"\\\\\\\\host-name\\\\share-name\\\\dir1\" . \"dir2\")
(on POSIX)
CL-USER > (split \"/foo/bar\")
=> (\"/foo\" . \"bar\")
CL-USER > (split \"/foo/bar/\")
=> (\"/foo/bar/\" . \"\")"
  #+windows (ppath.details.nt:split path)
  #-windows (ppath.details.posix:split path))


(defun splitdrive (path)
  "Split a path to the drive (with letter) and path after drive.
This function also parses the UNC paths, providing \\\\hostname\\mountpoint
as a drive part.

On POSIX drive is an empty string.

Invariant: (concatenate 'string (car (splitdrive path)) (cdr (splitdrive path))) == path

Example:
CL-USER > (splitdrive \"C:\\Sources\\lisp\")
=> (\"C:\" \"\\Sources\\lisp\")"
  #+windows (ppath.details.nt:splitdrive path)
  #-windows (cons "" path))

(defun splitext(path)
  "Split PATH to root and extension. Return a pair (root . ext)
If the filename component of the PATH starts with dot, like .cshrc, considering no extension.
Invariant: (concatenate 'string root ext) == path

Examples:
CL-USER > (splitext \"~/test.cshrc\")
=> (\"~/test\" . \".cshrc\")
CL-USER > (splitext \"~/notes.txt\")
=> (\"~/notes\" . \".txt\")"
  (ppath.details.generic:splitext path))


(defun splitparts (path)
  "Splits the path to the list of elements using. Separators are not omitted.
Example:
CL-USER > (splitparts \"/abc/def/gh//12\")
=> (\"/\" \"abc\" \"/\" \"def\" \"/\" \"gh\" \"//\" \"12\")"
  (ppath.details.generic:split-components path))


(defun splitunc (path)
  "Split a pathname with UNC path. UNC syntax:
\\\\host-name\\share-name\\file_path
Return a cons pair (\\\\host-name\\share-name . \\file_path)

Not available on POSIX."
  #+windows (ppath.details.nt:splitunc path)
  #-windows (declare (ignore path))
  #-windows (error "This function is not available on POSIX systems"))

