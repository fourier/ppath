[![Build Status](https://travis-ci.org/fourier/pypath.svg?branch=master)](https://travis-ci.org/fourier/pypath)
# A Common Lisp implementation of the Python's os.path module
Work in progress!!!

The intention of this package is to reimplemenent all the functions from Python's **path** module in Common Lisp. Author personally likes the design of this module and therefore decided to implement it.

Supported [tested] compilers: LispWorks (6.1PE and 7.0), CCL, SBCL.
Tested on the following platforms:

### Windows
- Clozure CLVersion 1.11-r16635  (WindowsX8664)
- LispWorks 6.1.1 Personal Edition 32bit
- LispWorks 7.0 Hobbyist Edition 32bit
- SBCL 1.3.15

Limitations: On Win32 assumed OS versions with Unicode support.

### Linux


## Implemented functions
 - Functions implemented for Win32 module:
   - **split**
   - **splitunc**
   - **splitdrive**
   - **split**
   - **splitunc**
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
   

## Not Implemented functions:
 - From ntpath module:
 - walk
 - samefile
 - samestat

## Author

* Alexey Veretennikov (alexey.veretennikov@gmail.com)

## Copyrights

 - pypath CL library Copyright (c) 2017 Alexey Veretennikov (alexey.veretennikov@gmail.com)
 - Python and Python documentation Copyright (c)  1990-2017, Python Software Foundation. 
 - Parts of Python documentation on https://docs.python.org/2.7/library/os.path.html were used and adapted when necessary to the current implementation.

## License

Licensed under the BSD License.
