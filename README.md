[![Build Status](https://travis-ci.org/fourier/ppath.svg?branch=master)](https://travis-ci.org/fourier/ppath)
# A Common Lisp path strings manipulation library.

This library is a path strings manipulation library inspired by Python's **os.path**. All functionality from **os.path** is supported on major operation systems (OSX, Linux, Windows).

The philosophy behind is to use simple strings and "dumb" string manipulation functions to handle paths and filenames. Where possible the corresponding OS system functions are called.

Supported [tested] compilers: LispWorks (6.1PE and 7.0), CCL, SBCL.
Tested on the following platforms:

### Windows
- Clozure CL Version 1.11-r16635  (WindowsX8664)
- LispWorks 6.1.1 Personal Edition 32bit
- LispWorks 7.0 Hobbyist Edition 32bit
- SBCL 1.3.15

Limitations: On Win32 assumed OS versions with Unicode support.

### Linux
- SBCL 1.3.14
- CCL 1.11.5

### OSX
- Lispworks 7.0 Hobbyist DV Edition 32bit
- Clozure CL Version 1.11

## Usage
The library consist of 3 packages: *ppath*, *ppath-nt* and *ppath-posix*. The generic package *ppath* forwards calls to appropriate OS-dependent library. It exports only functions available for the current platform.

## Semantical difference from Python's os.path module
- Where possible do not raise an error, but rather return nil.
- ```samestat``` function is not implemented (makes no sense since osicat's wrapper around stat()/fstat() calls is used)
- ```walk``` is not implemented (there is already a couple of good implementations around)

## API description

### *function* (ppath:abspath ```path```)
Convert relative PATH to absolute. If path is absolute return it unchanged,
if path is empty return current directory
    
On POSIX systems invariant:
(abspath path) == (normpath (join (getcwd) path))
holds

### (ppath:basename PATH)
  Derived type: (FUNCTION (T) *)
    Extract the base name (filename) of the PATH.
    
    Example:
    On Windows:
    CL-USER > (basename "C:\\dir\\file.txt")
    => file.txt
    
    On POSIX:
    CL-USER > (basename "/foo/bar")
    => bar
    
    Invariant: (basename path) == (cdr (split path))
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:COMMONPREFIX
  [symbol]

COMMONPREFIX names a compiled function:
  Lambda-list: (&REST PATHS)
  Derived type: FUNCTION
  Documentation:
    Get the common prefix substring  of all strings in PATHS. The separators are not translated,
    so paths interpreted just as normal strings.
    PATHS components could also be lists of strings, like results of
    SPLIT operation on paths. In this case the comparison happens elementwise.
    
    Example:
    CL-USER > (commonprefix '("/home/username/dir" "/home/user/test"))
    => /home/user
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:DIRNAME
  [symbol]

DIRNAME names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Get the directory name of the PATH. If the pt
    
    Example:
    CL-USER > (dirname "/foo/bar")
    => /foo
    
    Example (Windows):
    CL-USER > (dirname "C:\\dir\\file.txt")
    =>  C:\\dir
    
    Invariant: (dirname path) == (car (split path))
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:EXISTS
  [symbol]

EXISTS names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Check if the PATH is an existing path.
    On POSIX returns nil for broken symbolic links.
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:EXPANDUSER
  [symbol]

EXPANDUSER names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Expand ~ and ~user in the PATH with the contents of user's home path.
    ~ - home directory
    ~user - user's home directory
    Return PATH unchanged if unable to expand.
    
    On Windows the path is taken either from HOME or USERPROFILE, or constructed via HOMEPATH and HOMEDRIVE.
    On error just return original PATH value.
    On POSIX systems the ~ is replaced with contents of the HOME environment variable or taken from password database
    (/etc/passwd or similar).
    
    Examples (POSIX): (given the user "username" with home directory /Users/username)
    CL-USER > (expanduser "~/dir")
    => /Users/username/dir
    CL-USER > (expanduser "~root/dir")
    => /root/dir
    
    Windows: if HOMEPATH is "users\dir" and HOMEDRIVE is "C:\\",
    CL-USER > (expanduser "~test")
    => C:\users\test
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:EXPANDVARS
  [symbol]

EXPANDVARS names a compiled function:
  Lambda-list: (PATH &OPTIONAL (MODIFY-IN-QUOTES T))
  Derived type: (FUNCTION (T &OPTIONAL T) *)
  Documentation:
    Expand the PATH replacing environment variables with their contents.
    The variables like ${var} and $var (and additionally %var% on Windows) are getting replaced by their values.
    All unknown or malformed variables ignored and kept as it is.
    
    The difference between Windows and Posix systems is that on Windows variables inside single quotes are not
    expanded, i.e. "'$HOME'" will remain "'$HOME'", while on Posix systems it will be expanded. The optional argument MODIFY-IN-QUOTES allows to change this behavior.
    This behavior kept for compatibility with Python's os.path.expandvars.
    
    Example:
    CL-USER > (expandvars "$HOME/.bashrc")
    => /home/fourier/.bashrc
    
    CL-USER > (osicat-posix:setenv "foo" "abcd")
    => 0
    CL-USER > (expandvars "'$foo'$bar" nil)
    => '$foo'$bar
    CL-USER > (expandvars "'$foo'$bar" t)
    => 'abcd'$bar
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:GETATIME
  [symbol]

GETATIME names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return the last access time for the PATH.
    Return value is seconds since Unix Epoch (1970-01-01T00:00:00Z).
    Return nil if unable to access file or get its attributes.
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:GETCTIME
  [symbol]

GETCTIME names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return the last status change time for the PATH.
    Return value is seconds since Unix Epoch (1970-01-01T00:00:00Z).
    Return nil if unable to access file or get its attributes.
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:GETMTIME
  [symbol]

GETMTIME names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return the last modification time for the PATH.
    Return value is seconds since Unix Epoch (1970-01-01T00:00:00Z).
    Return nil if unable to access file or get its attributes.
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:GETSIZE
  [symbol]

GETSIZE names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Get the file size in bytes of the PATH.
    Return nil if unable to access file or get its attributes.
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:ISABS
  [symbol]

ISABS names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Determine if the PATH is an absolute pathname.
    This function never checks for file existance nor address
    file system but rather performs string manipulations to
    determine if the PATH is an absolute filename.
    
    Examples (POSIX):
    CL-USER > (isabs "/Sources/lisp")
    => t
    CL-USER > (isabs "my/dir")
    => nil
    
    Examples (Windows):
    CL-USER > isabs "\\\\host-name\\share-name\\")
    => t
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:ISDIR
  [symbol]

ISDIR names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Determine if PATH is an existing directory. If the PATH is symlink then the invariant
    (and (islink PATH) (isdir PATH)) holds.
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:ISFILE
  [symbol]

ISFILE names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Determine if the PATH exists and a file. Returns also t for symbolic links.
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:ISLINK
  [symbol]

ISLINK names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Determine if the PATH is symbolic link.
    On Windows always return nil.
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:ISMOUNT
  [symbol]

ISMOUNT names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Test if the path is a mount point.
    On POSIX it is a directory where another filesystem is mounted.
    On Windows for local paths it should be
    an absolute path, for UNC it should be mount point of the host
    
    Example:
    CL-USER > (ismount "/mnt")
    => nil
    CL-USER > (ismount "/mnt/cdrom")
    => t
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:JOIN
  [symbol]

JOIN names a compiled function:
  Lambda-list: (PATH &REST PATHS)
  Derived type: (FUNCTION (T &REST T) *)
  Documentation:
    Join paths provided, merging (if absolute) and
    inserting missing separators.
    
    Example:
    CL-USER > (join "a/b" "x/y")
    => a/b\\x/y
    CL-USER > (join "c:\\hello" "world/test.txt")
    => c:\\hello\\world/test.txt
    CL-USER > (join "/foo" "bar" "baz")
    => /foo/bar/baz
    CL-USER > (join "/foo/" "bar/" "baz/")
    => /foo/bar/baz/
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:LEXISTS
  [symbol]

LEXISTS names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Check if the PATH is an existing path.
    Checks for existance regardless if PATH is a link(even broken) or a file/directory.
    On Windows exists=lexists.
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:NORMCASE
  [symbol]

NORMCASE names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Normalize the PATH.
    On Windows, replace slash with backslahes and lowers the case of the PATH.
    On POSIX do nothing and just return PATH.
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:NORMPATH
  [symbol]

NORMPATH names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Normalize path, removing unnecessary/redundant parts, like dots,
    double slashes, etc. Expanding .. as well.
    
    Example:
    CL-USER > (normpath "///..//./foo/.//bar")
    => /foo/bar
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:REALPATH
  [symbol]

REALPATH names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Return real PATH of the file, following symlinks if necessary.
    On Windows just return (abspath path).
    The PATH shall be already expanded properly.
    Return nil if PATH does not exist or not accessible
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:RELPATH
  [symbol]

RELPATH names a compiled function:
  Lambda-list: (PATH &OPTIONAL (START .))
  Derived type: (FUNCTION (T &OPTIONAL T) *)
  Documentation:
    Return the relative version of the PATH.
    If STARTDIR specified, use this as a current directory to resolve against.
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:SAMEFILE
  [symbol]

SAMEFILE names a compiled function:
  Lambda-list: (PATH1 PATH2)
  Derived type: (FUNCTION (T T) *)
  Documentation:
    Determine if PATH1 and PATH2 are the paths to the same file.
    If one of the paths is symlink to another they considered the same.
    
    Not available on Windows.
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:SAMEOPENFILE
  [symbol]

SAMEOPENFILE names a compiled function:
  Lambda-list: (FP1 FP2)
  Derived type: (FUNCTION (T T) *)
  Documentation:
    Determine if the open file streams STREAM1 and STREAM2 are
    of the same file.
    
    Not available on Windows.
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:SPLIT
  [symbol]

SPLIT names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Split the path into the pair (directory . filename).
    If the path ends with "/", the file component is empty
    
    On Windows, if the head is a drive name, the slashes are not stripped from it.
    
    Examples:
    (On Windows)
    CL-USER > (split "c:\\Sources\\lisp")
    => ("c:\\Sources" . "lisp")
    CL-USER > (split "\\\\host-name\\share-name\\dir1\\dir2")
    => ("\\\\host-name\\share-name\\dir1" . "dir2")
    (on POSIX)
    CL-USER > (split "/foo/bar")
    => ("/foo" . "bar")
    CL-USER > (split "/foo/bar/")
    => ("/foo/bar/" . "")
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:SPLITDRIVE
  [symbol]

SPLITDRIVE names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) (VALUES CONS &OPTIONAL))
  Documentation:
    Split a path to the drive (with letter) and path after drive.
    This function also parses the UNC paths, providing \\hostname\mountpoint
    as a drive part.
    
    On POSIX drive is an empty string.
    
    Invariant: (concatenate 'string (car (splitdrive path)) (cdr (splitdrive path))) == path
    
    Example:
    CL-USER > (splitdrive "C:\Sources\lisp")
    => ("C:" "\Sources\lisp")
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:SPLITEXT
  [symbol]

SPLITEXT names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) *)
  Documentation:
    Split PATH to root and extension. Return a pair (root . ext)
    If the filename component of the PATH starts with dot, like .cshrc, considering no extension.
    Invariant: (concatenate 'string root ext) == path
    
    Examples:
    CL-USER > (splitext "~/test.cshrc")
    => ("~/test" . ".cshrc")
    CL-USER > (splitext "~/notes.txt")
    => ("~/notes" . ".txt")
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp
PPATH:SPLITUNC
  [symbol]

SPLITUNC names a compiled function:
  Lambda-list: (PATH)
  Derived type: (FUNCTION (T) NIL)
  Documentation:
    Split a pathname with UNC path. UNC syntax:
    \\host-name\share-name\file_path
    Return a cons pair (\\host-name\share-name . \file_path)
    
    Not available on POSIX.
  Source file: /home/fourier/Sources/lisp/ppath/src/ppath.lisp


## Author

* Alexey Veretennikov (alexey.veretennikov@gmail.com)

## Copyrights

 - Python and Python documentation Copyright (c)  1990-2017, Python Software Foundation. 
 - Parts of Python documentation on https://docs.python.org/2.7/library/os.path.html were used and adapted when necessary to the current implementation.

## License

Licensed under the BSD License.
