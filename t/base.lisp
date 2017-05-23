;;;; base.lisp
;; File containing common settings for all unit tests
;; 
(in-package :cl-user)
(defpackage pypath.test.base
  (:use :cl
        :prove)
  (:export testfile test-input with-mocked-function random-shuffle random-shufflef))
   
(in-package :pypath.test.base)

;; turn off ansi colors in report output
(setf prove.color:*enable-colors* nil)
;; change type of the reporter to Test Anything Protocol
(setf prove:*default-reporter* :tap)

(defvar *test-data-path* (fad:merge-pathnames-as-directory (asdf:system-relative-pathname :pypath-test #P"t/") #P"data/"))

(defun testfile (filename)
  (merge-pathnames filename *test-data-path*))


(defmacro test-input (fun inp out)
  ;; example of generated code:
  ;; (is (py.path.details.nt::splitdrive "C:\\")
  ;;     '("C:" . "\\")
	;;   :test #'equal
  ;;     "Testing input: 'C:\\'"))
  (let ((inp-string
         (format nil "Testing input: ~s" inp)))
    (if (listp inp)
        `(is (apply #',fun ,inp)
             ,out
             :test #'equal
             ,inp-string)
        `(is (,fun ,inp)
             ,out
             :test #'equal
             ,inp-string))))


(defmacro with-mocked-function ((old new) &body body)
  "Execute body with replacing function OLD to NEW.
Example:
(with-replaced-function 'mypackage::myfun (lambda (x) 123)
                        (mypackage::myfun))
=> 123
"
  (let ((oldf (gensym))
        (result (gensym)))
    `(let ((,oldf (symbol-function ',old)))
       (setf (symbol-function ',old) ,new)
       (let ((,result (progn ,@body)))
         (setf (symbol-function ',old) ,oldf)
         ,result))))


(defmethod random-shufflef ((container list))
  (let ((n (length container)))
    (loop for i from (- n 1) downto 0
          do 
          (rotatef (nth i container) (nth (random (+ i 1)) container)))
    container))


(defmethod random-shufflef ((container array))
  (let ((n (length container)))
    (loop for i from (- n 1) downto 0
          do 
          (rotatef (aref container i) (aref container (random (+ i 1)))))
    container))

(defmethod random-shuffle ((container list))
  (let ((result (copy-list container))
        (n (length container)))
    (loop for i from (- n 1) downto 0
          do 
          (rotatef (nth i result) (nth (random (+ i 1)) result)))
    result))


(defmethod random-shuffle ((container array))
  (let ((result (copy-seq container))
        (n (length container)))
    (loop for i from (- n 1) downto 0
          do 
          (rotatef (aref result i) (aref result (random (+ i 1)))))
    result))
