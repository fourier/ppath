[![Build Status](https://travis-ci.org/fourier/pypath.svg?branch=master)](https://travis-ci.org/fourier/pypath)
# A Common Lisp implementation of the Python's os.path module
Work in progress!!!

The intention of this package is to reimplemenent all the functions from Python's **path** module in Common Lisp. Author personally likes the design of this module and therefore decided to implement it.

Supported [tested] compilers: LispWorks (6.1PE and 7.0), CCL, SBCL.


## Implemented functions
 - From ntpath module:
   - **split**
   - **splitunc**
   - **splitdrive**
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
   - Not going to implement: **walk**

## Author

* Alexey Veretennikov (alexey.veretennikov@gmail.com)

## Copyright

Copyright (c) 2017 Alexey Veretennikov (alexey.veretennikov@gmail.com)

## License

Licensed under the BSD License.
