(defpackage py.path.details.posix.cffi
  (:use :cl :cffi :alexandria)
  (:export getpid))

(in-package py.path.details.posix.cffi)

(cffi:defcfun ("getpid" getpid) :int)

;; define necessary types used in
(defcstruct timespec
  (tv-sec :long)
  (tv-nsec :long))

(defctype dev_t :int32)
(defctype mode_t :uint16)
(defctype nlink_t :uint16)
(defctype ino_t :uint64)
(defctype uid_t :uint32)
(defctype gid_t :uint32)
(defctype off_t :int64)
(defctype blkcnt_t :int64)
(defctype blksize_t :int32)

;; constants for st_mode
(define-constant S_IFMT 0170000   :documentation "type of file")
(define-constant S_IFIFO 0010000  :documentation "named pipe (fifo)")
(define-constant S_IFCHR 0020000  :documentation "character special")
(define-constant S_IFDIR 0040000  :documentation "directory")
(define-constant S_IFBLK 0060000  :documentation "block special")
(define-constant S_IFREG 0100000  :documentation "regular")
(define-constant S_IFLNK 0120000  :documentation "symbolic link")
(define-constant S_IFSOCK 0140000 :documentation "socket")
(define-constant S_IFWHT 0160000  :documentation "whiteout")
(define-constant S_ISUID 0004000  :documentation "set user id on execution")
(define-constant S_ISGID 0002000  :documentation "set group id on execution")
(define-constant S_ISVTX 0001000  :documentation "save swapped text even after use")
(define-constant S_IRUSR 0000400  :documentation "read permission, owner")
(define-constant S_IWUSR 0000200  :documentation "write permission, owner")
(define-constant S_IXUSR 0000100  :documentation "execute/search permission, owner")

;; stat struct assuming _DARWIN_FEATURE_64_BIT_INODE is defined 
(defcstruct cstat
  (st-dev           dev_t)              ;; ID of device containing file
  (st-mode          mode_t)             ;; Mode of file (see below)
  (st-nlink         nlink_t)            ;; Number of hard links
  (st-ino           ino_t)              ;; File serial number
  (st-uid           uid_t)              ;; User ID of the file
  (st-gid           gid_t)              ;; Group ID of the file
  (st-rdev          dev_t)              ;; Device ID
  (st-atimespec     (:struct timespec)) ;; time of last access
  (st-mtimespec     (:struct timespec)) ;; time of last data modification
  (st-ctimespec     (:struct timespec)) ;; time of last status change
  (st-birthtimespec (:struct timespec)) ;; time of file creation(birth)
  (st-size          off_t)              ;; file size, in bytes
  (st-blocks        blkcnt_t)           ;; blocks allocated for file
  (st-blksize       blksize_t)          ;; optimal blocksize for I/O
  (st-flags         :uint32)            ;; user defined flags for file
  (st-gen           :uint32)            ;; file generation number
  (st-lspare        :int32)             ;;  RESERVED: DO NOT USE!
  (st-qspare        :int64 :count 2))   ;; RESERVED: DO NOT USE! 


;; int lstat(const char *restrict path, struct stat *restrict buf);
(cffi:defcfun ("lstat" c-lstat) :int
  (path :string)
  (buf  :pointer))

;; int stat(const char *restrict path, struct stat *restrict buf);
(cffi:defcfun ("stat" c-stat) :int
  (path :string)
  (buf  :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp wrappers around stat
(defstruct timespec
  (tv-sec  0 :type integer)
  (tv-nsec 0 :type integer))

;; same as c-struct stat
(defstruct stat
  (st-dev 0 :type integer)              ;; ID of device containing file
  (st-mode 0 :type integer)             ;; Mode of file (see below)
  (st-nlink 0 :type integer)            ;; Number of hard links
  (st-ino 0 :type integer)              ;; File serial number
  (st-uid 0 :type integer)              ;; User ID of the file
  (st-gid 0 :type integer)              ;; Group ID of the file
  (st-rdev 0 :type integer)             ;; Device ID
  (st-atimespec)                        ;; time of last access
  (st-mtimespec)                        ;; time of last data modification
  (st-ctimespec)                        ;; time of last status change
  (st-birthtimespec)                    ;; time of file creation(birth)
  (st-size 0 :type integer)              ;; file size, in bytes
  (st-blocks 0 :type integer)           ;; blocks allocated for file
  (st-blksize 0 :type integer)          ;; optimal blocksize for I/O
  (st-flags 0 :type integer)            ;; user defined flags for file
  (st-gen 0 :type integer))            ;; file generation number

(defun make-timespec-from-c-timespec (c-timespec)
  nil)

(defun make-stat-from-c-stat (c-stat)
  "Create Lisp STAT struct from CFFI stat struct"
#|
  (macrolet ((get-tv-sec (field)
                   `(foreign-slot-value 
                     (foreign-slot-pointer attr '(:struct stat)
                                           ',field)
                     '(:struct timespec) 'tv-sec))
             (get-tv-nsec (field)
               `(foreign-slot-value 
                 (foreign-slot-pointer attr '(:struct stat)
                                       ',field)
                 '(:struct timespec) 'tv-nsec)))
  |#
    (with-foreign-slots ((st-dev          
                          st-mode         
                          st-nlink        
                          st-ino          
                          st-uid          
                          st-gid          
                          st-rdev         
                          st-atimespec    
                          st-mtimespec    
                          st-ctimespec    
                          st-birthtimespec
                          st-size         
                          st-blocks       
                          st-blksize      
                          st-flags                                 
                          st-gen)
                         c-stat (:struct cstat))
      (make-stat
       :st-dev st-dev
       :st-mode (foreign-slot-value c-stat '(:struct cstat) 'st-mode);;st-mode
       :st-nlink st-nlink
       :st-ino st-ino
       :st-uid st-uid
       :st-gid st-gid
       :st-rdev st-rdev
       :st-atimespec (make-timespec-from-c-timespec st-atimespec)
       :st-mtimespec (make-timespec-from-c-timespec st-mtimespec)
       :st-ctimespec (make-timespec-from-c-timespec st-ctimespec)
       :st-birthtimespec (make-timespec-from-c-timespec st-birthtimespec)
       :st-size st-size
       :st-blocks st-blocks
       :st-blksize st-blksize
       :st-flags st-flags
       :st-gen st-gen)))

  
(defun get-stat (path)
  "Return an instance of STAT structure for given file PATH"
  (with-foreign-string (c-path path :null-terminated-p t)
    (with-foreign-object (buf '(:struct cstat))
      (let ((result (c-stat c-path buf)))
        (when (= result 0) ; not failed to get attributes
          (make-stat-from-c-stat buf))))))
