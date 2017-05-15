(defpackage py.path
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


(in-package py.path)
;;(proclaim '(optimize (safety 0) (speed 3)))
(proclaim '(optimize (safety 3) (speed 0) (debug 3)))

(defun empty (str)
  "Returns t if the string is empty"
  (or (not str)
      (= (length str) 0)))

(defun abspath(path)
  "Return a normalized absolutized version of the pathname path. On most platforms, this is equivalent to calling the function normpath() as follows: normpath(join(os.getcwd(), path))."
  )

(defun basename(path)
  "Return the base name of pathname path. This is the second element of the pair returned by passing path to the function split(). Note that the result of this function is different from the Unix basename program; where basename for '/foo/bar/' returns 'bar', the basename() function returns an empty string ('')."
  )

(defun commonprefix(list)
  "Return the longest path prefix (taken character-by-character) that is a prefix of all paths in list. If list is empty, return the empty string (''). Note that this may return invalid paths because it works a character at a time."
  )

(defun dirname(path)
  "Return the directory name of pathname path. This is the first element of the pair returned by passing path to the function split()."
  )

(defun exists(path)
  "Return True if path refers to an existing path. Returns False for broken symbolic links. On some platforms, this function may return False if permission is not granted to execute os.stat() on the requested file, even if the path physically exists."
  )

(defun lexists(path)
  "Return True if path refers to an existing path. Returns True for broken symbolic links. Equivalent to exists() on platforms lacking os.lstat()."
  )

(defun expanduser(path)
  "On Unix and Windows, return the argument with an initial component of ~ or ~user replaced by that user‘s home directory.

On Unix, an initial ~ is replaced by the environment variable HOME if it is set; otherwise the current user’s home directory is looked up in the password directory through the built-in module pwd. An initial ~user is looked up directly in the password directory.

On Windows, HOME and USERPROFILE will be used if set, otherwise a combination of HOMEPATH and HOMEDRIVE will be used. An initial ~user is handled by stripping the last directory component from the created user path derived above.

    If the expansion fails or if the path does not begin with a tilde, the path is returned unchanged."
  )
  
(defun expandvars(path)
  "Return the argument with environment variables expanded. Substrings of the form $name or ${name} are replaced by the value of environment variable name. Malformed variable names and references to non-existing variables are left unchanged.

On Windows, %name% expansions are supported in addition to $name and ${name}."
    )
    
(defun getatime(path)
  "Return the time of last access of path. The return value is a number giving the number of seconds since the epoch (see the time module). Raise os.error if the file does not exist or is inaccessible."
  )

(defun getmtime(path)
  "Return the time of last modification of path. The return value is a number giving the number of seconds since the epoch (see the time module). Raise os.error if the file does not exist or is inaccessible."
  )

(defun getctime(path)
  "Return the system’s ctime which, on some systems (like Unix) is the time of the last metadata change, and, on others (like Windows), is the creation time for path. The return value is a number giving the number of seconds since the epoch (see the time module). Raise os.error if the file does not exist or is inaccessible."
  )


(defun getsize(path)
  "return the size, in bytes, of path. raise os.error if the file does not exist or is inaccessible."
  )

(defun isabs(path)
  "return true if path is an absolute pathname. on unix, that means it begins with a slash, on windows that it begins with a (back)slash after chopping off a potential drive letter."
  )

(defun isfile(path)
  "return true if path is an existing regular file. this follows symbolic links, so both islink() and isfile() can be true for the same path."
  )

(defun isdir(path)
  "return true if path is an existing directory. this follows symbolic links, so both islink() and isdir() can be true for the same path."
  )

(defun islink(path)
  "return true if path refers to a directory entry that is a symbolic link. always false if symbolic links are not supported by the python runtime."
  )

(defun ismount(path)
  "return true if pathname path is a mount point: a point in a file system where a different file system has been mounted. the function checks whether path‘s parent, path/.., is on a different device than path, or whether path/.. and path point to the same i-node on the same device — this should detect mount points for all unix and posix variants."
  )


(defun join(path &rest paths)
  "join one or more path components intelligently. the return value is the concatenation of path and any members of *paths with exactly one directory separator (os.sep) following each non-empty part except the last, meaning that the result will only end in a separator if the last part is empty. if a component is an absolute path, all previous components are thrown away and joining continues from the absolute path component.

On windows, the drive letter is not reset when an absolute path component (e.g., r'\foo') is encountered. if a component contains a drive letter, all previous components are thrown away and the drive letter is reset. note that since there is a current directory for each drive, os.path.join(\"c:\", \"foo\") represents a path relative to the current directory on drive c: (c:foo), not c:\foo."
  )


(defun normcase(path)
  "normalize the case of a pathname. on unix and mac os x, this returns the path unchanged; on case-insensitive filesystems, it converts the path to lowercase. on windows, it also converts forward slashes to backward slashes."
  )

(defun normpath(path)
  "normalize a pathname by collapsing redundant separators and up-level references so that a//b, a/b/, a/./b and a/foo/../b all become a/b. this string manipulation may change the meaning of a path that contains symbolic links. on windows, it converts forward slashes to backward slashes. to normalize case, use normcase()."
  )


(defun realpath(path)
  "return the canonical path of the specified filename, eliminating any symbolic links encountered in the path (if they are supported by the operating system)."
  )

(defun relpath (path &optional start)
  "return a relative filepath to path either from the current directory or from an optional start directory. this is a path computation: the filesystem is not accessed to confirm the existence or nature of path or start.

start defaults to os.curdir."
    )

(defun samefile(path1 path2)
  "return true if both pathname arguments refer to the same file or directory (as indicated by device number and i-node number). raise an exception if a os.stat() call on either pathname fails."
  )

(defun sameopenfile(fp1 fp2)
  "return true if the file descriptors fp1 and fp2 refer to the same file."
  )


(defun samestat(stat1 stat2)
  "return true if the stat tuples stat1 and stat2 refer to the same file. these structures may have been returned by os.fstat(), os.lstat(), or os.stat(). this function implements the underlying comparison used by samefile() and sameopenfile()."
  )

(defun split(path)
  "split the pathname path into a pair, (head, tail) where tail is the last pathname component and head is everything leading up to that. the tail part will never contain a slash; if path ends in a slash, tail will be empty. if there is no slash in path, head will be empty. if path is empty, both head and tail are empty. trailing slashes are stripped from head unless it is the root (one or more slashes only). in all cases, join(head, tail) returns a path to the same location as path (but the strings may differ). also see the functions dirname() and basename()."
#|
  (let ((splitted (lw:split-sequence +separator-str+ path)))
    (if (every #'empty splitted) ; all empty, return the root string and empty tail
        (cons +separator-str+ "")
        (concatenate 'string (butlast splitted))
        |#
  )

(defun splitdrive(path)
  "split the pathname path into a pair (drive, tail) where drive is either a drive specification or the empty string. on systems which do not use drive specifications, drive will always be the empty string. in all cases, drive + tail will be the same as path."
  )

(defun splitext(path)
  "split the pathname path into a pair (root, ext) such that root + ext == path, and ext is empty or begins with a period and contains at most one period. leading periods on the basename are ignored; splitext('.cshrc') returns ('.cshrc', '')."
  )

(defun splitunc(path)
  "split the pathname path into a pair (unc, rest) so that unc is the unc mount point (such as r'\\host\mount'), if present, and rest the rest of the path (such as r'\path\file.ext'). for paths containing drive letters, unc will always be the empty string."
  )

(defun walk(path visit arg)
  "calls the function visit with arguments (arg, dirname, names) for each directory in the directory tree rooted at path (including path itself, if it is a directory). the argument dirname specifies the visited directory, the argument names lists the files in the directory (gotten from os.listdir(dirname)). the visit function may modify names to influence the set of directories visited below dirname, e.g. to avoid visiting certain parts of the tree. (the object referred to by names must be modified in place, using del or slice assignment.)

    note

    symbolic links to directories are not treated as subdirectories, and that walk() therefore will not visit them. to visit linked directories you must identify them with os.path.islink(file) and os.path.isdir(file), and invoke walk() as necessary."
  )


            
