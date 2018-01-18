(in-package ppath.details.posix.cffi.package)

(defwrapper ("stat" c-stat) ("int" :int)
  (file-name ("const char*" :string))
  (buf ("struct stat*" :pointer)))

(defwrapper ("fstat" c-fstat) ("int" :int)
  (filedes ("int" :string))
  (buf ("struct stat*" :pointer)))

#-windows
(defwrapper ("lstat" c-lstat) ("int" :int)
  (file-name ("const char*" :string))
  (buf ("struct stat*" :pointer)))

