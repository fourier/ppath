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
- Where possible do not raise an error, but rather return ```nil```.
- ```samestat``` function is not implemented (makes no sense since osicat's wrapper around stat()/fstat() calls is used)
- ```walk``` is not implemented (there is already a couple of good implementations around)

## API description

### *function* ppath:abspath (```path```)
Convert relative ```path``` to absolute.
If path is absolute return it unchanged.
If path is empty return current directory.
    
On POSIX systems invariant:
```(abspath path) == (normpath (join (getcwd) path))```



### *function* ppath:basename (```path```)
Extract the base name (filename) of the ```path```.
    
Example:
On Windows:
```lisp
CL-USER > (basename "C:\\dir\\file.txt")
=> file.txt
```

On POSIX:
```lisp
CL-USER > (basename "/foo/bar")
=> bar
```

Invariant:
```(basename path) == (cdr (split path))```


### *function* ppath:commonprefix (```&rest paths```)
Get the common prefix substring  of all strings in ```paths```. The separators are not translated, so paths interpreted just as normal strings.

```paths``` components could also be lists of strings, like results of ```split``` operation on paths. In this case the comparison happens elementwise.
    
Example:
    
```lisp
CL-USER > (commonprefix '("/home/username/dir" "/home/user/test"))
=> /home/user
```
    
### *function* ppath:dirname (```path```)
Get the directory name of the ```path```.
    
Example:
    
```lisp
CL-USER > (dirname "/foo/bar")
=> /foo
```

Example (Windows):

```lisp
CL-USER > (dirname "C:\\dir\\file.txt")
=>  C:\\dir
```    
Invariant: ```(dirname path) == (car (split path))```

### *function* ppath:exists (```path```)
Check if the ```path``` is an existing path.
On POSIX returns ```nil``` for broken symbolic links.


### *function* ppath:expanduser (```PATH```)
Expand ~ and ~user in the ```path``` with the contents of user's home path.

* ~ - home directory
* ~user - user's home directory

Return ```path``` unchanged if unable to expand.
    
On Windows the path is taken either from **HOME** or **USERPROFILE**, or constructed via **HOMEPATH** and **HOMEDRIVE**.
On error just return original ```path``` value.
On POSIX systems the ~ is replaced with contents of the **HOME** environment variable or taken from password database (```/etc/passwd``` or similar).
    
Examples (POSIX): (given the user "username" with home directory /Users/username)

```lisp
CL-USER > (expanduser "~/dir")
=> /Users/username/dir
CL-USER > (expanduser "~root/dir")
=> /root/dir
```

Windows: if **HOMEPATH** is "users\dir" and **HOMEDRIVE** is "C:\\",

```lisp
CL-USER > (expanduser "~test")
=> C:\users\test
```

### *function* ppath:expandvars (```path &optional (modify-in-quotes t)```)

Expand the ```path``` replacing environment variables with their contents.

The variables like ```${var}``` and ```$var``` (and additionally ```%var%``` on Windows) are getting replaced by their values.

All unknown or malformed variables ignored and kept as it is.
    
The difference between Windows and POSIX systems is that on Windows variables inside single quotes are not expanded, i.e. "```'$HOME'```" will remain "```'$HOME'```", while on POSIX systems it will be expanded. The optional argument ```modify-in-quotes``` allows to change this behavior.

This behavior kept for compatibility with Python's ```os.path.expandvars```.

Example:
```lisp
CL-USER > (expandvars "$HOME/.bashrc")
=> /home/username/.bashrc
CL-USER > (osicat-posix:setenv "foo" "abcd")
=> 0
CL-USER > (expandvars "'$foo'$bar" nil)
=> '$foo'$bar
CL-USER > (expandvars "'$foo'$bar" t)
=> 'abcd'$bar
```

### *function* ppath:getatime (```path```)
Return the last access time for the ```path```.

Return value is seconds since Unix Epoch (1970-01-01T00:00:00Z).

Return ```nil``` if unable to access file or get its attributes.

### *function* ppath:getctime (```path```)
Return the last status change time for the ```path```.

Return value is seconds since Unix Epoch (1970-01-01T00:00:00Z).

Return ```nil``` if unable to access file or get its attributes.

### *function* ppath:getmtime (```path```)
Return the last modification time for the ```path```.

Return value is seconds since Unix Epoch (1970-01-01T00:00:00Z).

Return ```nil``` if unable to access file or get its attributes.

### *function* ppath:getsize (```path```)
Get the file size in bytes of the ```path```.

Return ```nil``` if unable to access file or get its attributes.

### *function* ppath:isabs (```path```)
Determine if the ```path``` is an absolute pathname.

This function never checks for file existance nor address
file system but rather performs string manipulations to
determine if the ```path``` is an absolute filename.
    
Examples (POSIX):
```lisp
CL-USER > (isabs "/Sources/lisp")
=> t
CL-USER > (isabs "my/dir")
=> nil
```

Examples (Windows):
```lisp
CL-USER > (isabs "\\\\host-name\\share-name\\")
=> t
```

### *function* ppath:isdir (```path```)
Determine if ```path``` is an existing directory. If the ```path``` is symlink then the invariant
```(and (islink path) (isdir path)) == t```
holds.

### *function* ppath:isfile (```path```)
Determine if the ```path``` exists and a file. Returns also ```t``` for symbolic links.
    
### *function* ppath:islink (```path```)
Determine if the ```path``` is symbolic link.

On Windows always return ```nil```.

### *function* ppath:ismount (```path```)
Test if the ```path``` is a mount point.

On POSIX it is a directory where another filesystem is mounted.

On Windows for local paths it should be an absolute path, for UNC it should be mount point of the host
    
Example:
```lisp
CL-USER > (ismount "/mnt")
=> nil
CL-USER > (ismount "/mnt/cdrom")
=> t
```

### *function* ppath:join (```path &rest paths```)
Join paths provided, merging (if absolute) and
inserting missing separators.

Example:
```lips
CL-USER > (join "a/b" "x/y")
=> a/b\\x/y
CL-USER > (join "c:\\hello" "world/test.txt")
=> c:\\hello\\world/test.txt
CL-USER > (join "/foo" "bar" "baz")
=> /foo/bar/baz
CL-USER > (join "/foo/" "bar/" "baz/")
=> /foo/bar/baz/
```

### *function* ppath:lexists (```path```)
Check if the ```path``` is an existing path.

Checks for existance regardless if ```path``` is a link(even broken) or a file/directory.

On Windows ```exists``` = ```lexists```.

### *function* ppath:normcase (```path```)
Normalize the ```path```.

On Windows, replace slash with backslahes and lowers the case of the ```path```.

On POSIX do nothing and just return ```path```.

### *function* ppath:normpath (```path```)
Normalize path, removing unnecessary/redundant parts, like dots, double slashes, etc. Expanding ```..``` as well.
    
Example:
```lisp    
CL-USER > (normpath "///..//./foo/.//bar")
=> /foo/bar
```

### *function* ppath:realpath (```path```)
Return real ```path``` of the file, following symlinks if necessary. On Windows just return (abspath path). The ```path``` shall be already expanded properly.

Return ```nil``` if ```path``` does not exist or not accessible

### *function* ppath:relpath (```path &optional (start ".")```)
Return the relative version of the ```path```.

If ```startdir``` provided, use this as a current directory to resolve against.

### *function* ppath:samefile (```path1 path2```)
Determine if ```path1``` and ```path2``` are the paths to the same file.
If one of the paths is symlink to another they considered the same.
    
*Not available on Windows.*

### *function* ppath:sameopenfile (```stream1 stream2```)
Determine if the open file streams ```stream1``` and ```stream2``` are of the same file.
    
*Not available on Windows.*
    
### *function* ppath:split (```path```)
Split the path into the pair ```(directory . filename)```.
If the path ends with "/", the file component is empty.
    
On Windows, if the head is a drive name, the slashes are not stripped from it.
    
Examples:
(On Windows)
```lisp
CL-USER > (split "c:\\Sources\\lisp")
=> ("c:\\Sources" . "lisp")
CL-USER > (split "\\\\host-name\\share-name\\dir1\\dir2")
=> ("\\\\host-name\\share-name\\dir1" . "dir2")
```

(on POSIX)
```lisp
CL-USER > (split "/foo/bar")
=> ("/foo" . "bar")
CL-USER > (split "/foo/bar/")
=> ("/foo/bar/" . "")
```

### *function* ppath:splitdrive (```path```)
Split a ```path``` to the drive (with letter) and path after the drive.

This function also parses the UNC paths, providing \\hostname\mountpoint as a drive part.
    
On POSIX drive is an empty string.
    
Invariant: ```(concatenate 'string (car (splitdrive path)) (cdr (splitdrive path))) == path```
    
Example:
```lisp
CL-USER > (splitdrive "C:\Sources\lisp")
=> ("C:" "\Sources\lisp")
```

### *function* ppath:splitext (```path```)
Split ```path``` to root and extension. Return a pair ```(root . ext)```

If the filename component of the ```path``` starts with dot, like ```.cshrc```, considering no extension.

Invariant: ```(concatenate 'string root ext) == path```
    
Examples:
```lisp
CL-USER > (splitext "~/test.cshrc")
=> ("~/test" . ".cshrc")
CL-USER > (splitext "~/notes.txt")
=> ("~/notes" . ".txt")
```

### *function* ppath:splitunc (```path```)
Split a pathname with UNC path. UNC syntax: ```\\host-name\share-name\file_path```

Return a cons pair ```("\\host-name\share-name" . "\file_path")```
    
*Not available on POSIX.*
    

## Author

* Alexey Veretennikov (alexey.veretennikov@gmail.com)

## Copyrights

 - Python and Python documentation Copyright (c)  1990-2017, Python Software Foundation. 
 - Parts of Python documentation on https://docs.python.org/2.7/library/os.path.html were used and adapted when necessary to the current implementation.

## License

Licensed under the BSD License.
