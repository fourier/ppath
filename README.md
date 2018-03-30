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

## Implemented functions
 - Functions implemented for Win32 module:
   - **split**
   - **splitunc**
   - **splitdrive**
   - **isabs**
   - **normcase**
   - **basename**
   - **dirname**
   - **islink**
   - **ismount**
   - **join**
   - **expanduser**
   - **expandvars**
   - **normpath**
   - **abspath**
   - **realpath**
   - **relpath**
   - **isdir**
   - **getsize**
   - **getatime**
   - **getmtime**
   - **getctime**
   - **exists**
   - **isfile**
 - Functions implemented for Posix module:
   - **join**
   - **split**
   - **isabs**
   - **normcase**
   - **basename**
   - **dirname**
   - **islink**
   - **exists**
   - **lexists**
   - **samestat**
   - **samefile**
   - **sameopenfile**
   - **expanduser**
   - **expandvars**
   - **normpath**
   - **abspath**
   - **relpath**
   - **realpath**
   - **ismount**

## Not Implemented functions:
 - samestat (makes no sense since osicat's wrapper around stat()/fstat() 
   calls is used)
 - walk (there is already a couple of good implementations around)

## Author

* Alexey Veretennikov (alexey.veretennikov@gmail.com)

## Copyrights

 - Python and Python documentation Copyright (c)  1990-2017, Python Software Foundation. 
 - Parts of Python documentation on https://docs.python.org/2.7/library/os.path.html were used and adapted when necessary to the current implementation.

## License

Licensed under the BSD License.
