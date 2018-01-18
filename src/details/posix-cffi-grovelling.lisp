(in-package ppath.details.posix.cffi.package)

(include "string.h" "errno.h"  "sys/types.h" "sys/stat.h" "time.h")

(cstruct timespec "struct timespec"
   (tv_sec      "tv_sec"  :type :long)
   (tv_nsec     "tv_nsec" :type :long))

(ctype dev_t "dev_t")
(ctype mode_t "mode_t")
(ctype nlink_t "nlink_t")
(ctype ino_t "ino_t")
(ctype uid_t "uid_t")
(ctype gid_t "gid_t")
(ctype off_t "off_t")
(ctype blkcnt_t "blkcnt_t")
(ctype blksize_t "blksize_t")



(cstruct stat "struct stat"
  (dev           "st_dev"           :type dev_t)          ;; ID of device containing file
  (mode          "st_mode"          :type mode_t)         ;; Mode of file (see below) 
  (nlink         "st_nlink"         :type nlink_t)        ;; Number of hard links 
  (ino           "st_ino"           :type ino_t)          ;; File number serial 
  (uid           "st_uid"           :type uid_t)          ;; User ID of the file 
  (gid           "st_gid"           :type gid_t)          ;; Group ID of the file 
  (rdev          "st_rdev"          :type dev_t)          ;; Device ID 
  (atimespec     "st_atimespec"     :type timespec)       ;; time of last access 
  (mtimespec     "st_mtimespec"     :type timespec)       ;; time of last data modification 
  (ctimespec     "st_ctimespec"     :type timespec)       ;; time of last status change 
  (birthtimespec "st_birthtimespec" :type timespec)       ;; time of file creation(birth) 
  (size          "st_size"          :type off_t)          ;; file size, in bytes 
  (blocks        "st_blocks"        :type blkcnt_t)       ;; blocks allocated for file 
  (blksize       "st_blksize"       :type blksize_t)      ;; optimal blocksize for I/O 
  (flags         "st_flags"         :type :uint32)         ;; user defined flags for file 
  (gen           "st_gen"           :type :uint32)         ;; file generation number 
  (lspare        "st_lspare"        :type :int32)          ;; RESERVED: DO NOT USE! 
  (qspare        "st_qspare"        :type :int64 :count 2));; RESERVED: DO NOT USE! 
