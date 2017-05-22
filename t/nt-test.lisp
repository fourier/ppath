;;;; attributes-test.lisp
;; NOTE: To run this test file, execute `(asdf:test-system :git-api)' in your Lisp.
;;

(in-package :cl-user)
(defpackage py.path.test.nt-test
  (:use :cl
		:alexandria
        :py.path.details.nt
        :pypath.test.base
        :prove)
  (:shadowing-import-from py.path.details.nt
                          split
                          splitdrive
                          splitunc
                          isabs
                          normcase
                          basename
                          dirname
						  join))
(in-package :py.path.test.nt-test)

;; NOTE: To run this test file, execute `(asdf:test-system :pypath)' in your Lisp.

(plan nil)

  
(subtest "Test splitdrive"
  (test-input splitdrive "C:\\" '("C:" . "\\"))
  (test-input splitdrive "C:\\Sources\\lisp" '("C:" . "\\Sources\\lisp"))
  (test-input splitdrive "C:/Sources/lisp" '("C:" . "/Sources/lisp"))
  (test-input splitdrive "//host-name/share-name/dir" '("//host-name/share-name" . "/dir"))
  (test-input splitdrive "\\\\host-name\\share-name\\dir" '("\\\\host-name\\share-name" . "\\dir"))
  (test-input splitdrive "dir1\\dir2" '("" . "dir1\\dir2"))
  (test-input splitdrive "\\\\host-name\\share-name\\" '("\\\\host-name\\share-name" . "\\"))
  (test-input splitdrive "\\\\host-name\\share-name" '("\\\\host-name\\share-name" . "")))


(subtest "Test split"
  (test-input split "c:\\Sources\\lisp" '("c:\\Sources" . "lisp"))
  (test-input split "\\\\host-name\\share-name\\dir1\\dir2"
              '("\\\\host-name\\share-name\\dir1" . "dir2"))
  (test-input split "c:\\" '("c:\\" . ""))
  (test-input split "\\\\host-name\\share-name\\" '("\\\\host-name\\share-name\\" . ""))
  (test-input split "c:/" '("c:/" . ""))
  (test-input split "//host-name/share-name/" '("//host-name/share-name/" . ""))
  (test-input split "//host-name/share-name" '("//host-name/share-name" . "")))


(subtest "Test splitunc"
  (test-input splitunc "c:\\Sources\\lisp" '("" . "c:\\Sources\\lisp"))
  (test-input splitunc "c:/Sources/lisp" '("" . "c:/Sources/lisp"))
  (test-input splitunc "\\\\host-name\\share-name\\Sources\\lisp"
              '("\\\\host-name\\share-name" . "\\Sources\\lisp"))
  (test-input splitunc "//host-name/share-name/Sources/lisp"
              '("//host-name/share-name" . "/Sources/lisp"))
  (test-input splitunc "\\\\\\host-name\\share-name\\Sources\\lisp"
              '("" . "\\\\\\host-name\\share-name\\Sources\\lisp"))
  (test-input splitunc "///host-name/share-name/Sources/lisp"
              '("" . "///host-name/share-name/Sources/lisp"))
  (test-input splitunc "\\\\host-name\\\\share-name\\Sources\\lisp"
              '("" . "\\\\host-name\\\\share-name\\Sources\\lisp"))
  (test-input splitunc "//host-name//share-name/Sources/lisp"
              '("" . "//host-name//share-name/Sources/lisp"))
  (test-input splitunc "//host-name/share-name"
              '("//host-name/share-name" . ""))
  (test-input splitunc "//host-name/share-name/"
              '("//host-name/share-name" . "/")))



(subtest "Test isabs"
  (test-input isabs "c:\\" t)
  (test-input isabs "\\Sources" t)
  (test-input isabs "/Sources/lisp" t)
  (test-input isabs "\\\\host-name\\share-name\\" t)
  (test-input isabs "my/dir" nil)
  (test-input isabs "my\\dir1\dir2" nil)
  (test-input isabs "file.txt" nil))


(subtest "Test normcase"
  (test-input normcase "C:\\" "c:/"))


(subtest "Test basename"
  (test-input basename "C:\\dir\\file.txt" "file.txt"))


(subtest "Test dirname"
  (test-input dirname "C:\\dir\\file.txt" "C:\\dir"))


(subtest "Test ismount"
  (test-input ismount "C:\\dir\\file.txt" nil)
  (test-input ismount "\\\\host-name\\share-name" t)
  (test-input ismount "//host-name/share-name" t)
  (test-input ismount "//host-name/share-name/" t)
  (test-input ismount "//host-name/share-name/dir" nil)
  (test-input ismount "local-path\\dir" nil))
 

(subtest "Test join"
  (test-input join "" "")
  (test-input join '("" "" "") "")
  (test-input join '("a") "a")
  (test-input join '("/a") "/a")
  (test-input join '("\\a") "\\a")
  (test-input join '("a:") "a:")
  (test-input join '("a:" "\\b") "a:\\b")
  (test-input join '("a" "\\b") "\\b")
  (test-input join '("a" "b" "c") "a\\b\\c")
  (test-input join '("a\\" "b" "c") "a\\b\\c")
  (test-input join '("a" "b\\" "c") "a\\b\\c")
  (test-input join '("a" "b" "\\c") "\\c")
  (test-input join '("d:\\" "\\a") "d:\\a")
  (test-input join '("d:\\" "a" "b") "d:\\a\\b")

  (test-input join '("" "a") "a")
  (test-input join '("" "" "" "" "a") "a")
  (test-input join '("a" "") "a\\")
  (test-input join '("a" "" "" "" "") "a\\")
  (test-input join '("a\\" "") "a\\")
  (test-input join '("a\\" "" "" "" "") "a\\")
  (test-input join '("a/" "") "a/")

  (test-input join '("a/b" "x/y") "a/b\\x/y")
  (test-input join '("/a/b" "x/y") "/a/b\\x/y")
  (test-input join '("/a/b/" "x/y") "/a/b/x/y")
  (test-input join '("c:" "x/y") "c:x/y")
  (test-input join '("c:a/b" "x/y") "c:a/b\\x/y")
  (test-input join '("c:a/b/" "x/y") "c:a/b/x/y")
  (test-input join '("c:/" "x/y") "c:/x/y")
  (test-input join '("c:/a/b" "x/y") "c:/a/b\\x/y")
  (test-input join '("c:/a/b/" "x/y") "c:/a/b/x/y")
  (test-input join '("//host-name/share-name" "x/y") "//host-name/share-name\\x/y")
  (test-input join '("//host-name/share-name/" "x/y") "//host-name/share-name/x/y")
  (test-input join '("//host-name/share-name/a/b" "x/y") "//host-name/share-name/a/b\\x/y")

  (test-input join '("a/b" "/x/y") "/x/y")
  (test-input join '("/a/b" "/x/y") "/x/y")
  (test-input join '("c:" "/x/y") "c:/x/y")
  (test-input join '("c:a/b" "/x/y") "c:/x/y")
  (test-input join '("c:/" "/x/y") "c:/x/y")
  (test-input join '("c:/a/b" "/x/y") "c:/x/y")
  (test-input join '("//host-name/share-name" "/x/y") "//host-name/share-name/x/y")
  (test-input join '("//host-name/share-name/" "/x/y") "//host-name/share-name/x/y")
  (test-input join '("//host-name/share-name/a" "/x/y") "//host-name/share-name/x/y")

  (test-input join '("c:" "C:x/y") "C:x/y")
  (test-input join '("c:a/b" "C:x/y") "C:a/b\\x/y")
  (test-input join '("c:/" "C:x/y") "C:/x/y")
  (test-input join '("c:/a/b" "C:x/y") "C:/a/b\\x/y")

  (map-product (lambda (x y)
				 (is (apply #'join (list x y))
					 y
					 :test #'equal
					 (format nil "Testing combination: join (~s ~s) == ~s" x y y)))
			   '("" "a/b" "/a/b" "c:" "c:a/b" "c:/" "c:/a/b")
			   '("d:" "d:x/y" "d:/" "d:/x/y")))


;; (subtest "Test expanduser"
;;   (setf (osicat:environment-variable "HOME") "")
  
            ;; env.clear()
            ;; tester('ntpath.expanduser("~test")', '~test')

            ;; env['HOMEPATH'] = 'eric\\idle'
            ;; env['HOMEDRIVE'] = 'C:\\'
            ;; tester('ntpath.expanduser("~test")', 'C:\\eric\\test')
            ;; tester('ntpath.expanduser("~")', 'C:\\eric\\idle')

            ;; del env['HOMEDRIVE']
            ;; tester('ntpath.expanduser("~test")', 'eric\\test')
            ;; tester('ntpath.expanduser("~")', 'eric\\idle')

            ;; env.clear()
            ;; env['USERPROFILE'] = 'C:\\eric\\idle'
            ;; tester('ntpath.expanduser("~test")', 'C:\\eric\\test')
            ;; tester('ntpath.expanduser("~")', 'C:\\eric\\idle')

            ;; env.clear()
            ;; env['HOME'] = 'C:\\idle\\eric'
            ;; tester('ntpath.expanduser("~test")', 'C:\\idle\\test')
            ;; tester('ntpath.expanduser("~")', 'C:\\idle\\eric')

            ;; tester('ntpath.expanduser("~test\\foo\\bar")',
            ;;        'C:\\idle\\test\\foo\\bar')
            ;; tester('ntpath.expanduser("~test/foo/bar")',
            ;;        'C:\\idle\\test/foo/bar')
            ;; tester('ntpath.expanduser("~\\foo\\bar")',
            ;;        'C:\\idle\\eric\\foo\\bar')
            ;; tester('ntpath.expanduser("~/foo/bar")',
            ;;        'C:\\idle\\eric/foo/bar')
;;)  

(finalize)
