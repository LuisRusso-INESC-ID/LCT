;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload "cffi")

(ql:quickload "split-sequence")
;; (princ
;; 	(split-sequence:SPLIT-SEQUENCE #\Space "A stitch in time saves nine.") )

(defpackage :cffi-lct
  (:use :common-lisp :cffi))

(in-package :cffi-lct)

;; (define-foreign-library lct
;;   (:unix "./pointerLCT.so")
;;   (t (:default "./pointerLCT.so")))

(define-foreign-library lct
  (:unix "./splayLCT.so")
  (t (:default "./splayLCT.so")))

(use-foreign-library lct)

;; (defcfun "initLCT" :void
;;   (f (:pointer))
;;   (n :unsigned-int))
;;
;; (defcfun "sizeofStruct" :unsigned-int)

(defcfun "allocLCT" :pointer
  (n :unsigned-int))

(defcfun "Access" :unsigned-int
  (f :pointer)
  (n :unsigned-int))

(defcfun "getNode" :unsigned-int
  (f :pointer)
  (v :unsigned-int)
  (d :unsigned-int))

(defcfun "edgeQ" :int
  (f :pointer)
  (u :unsigned-int)
  (v :unsigned-int))

(defcfun "cut" :unsigned-int
  (f :pointer)
  (u :unsigned-int)
  (v :unsigned-int))

(defcfun "reRoot" :void
  (f :pointer)
  (v :unsigned-int))

(defcfun "LCA" :unsigned-int
  (f :pointer)
  (u :unsigned-int)
  (v :unsigned-int))

(defcfun "Link" :void
  (f :pointer)
  (u :unsigned-int)
  (v :unsigned-int))

(defcfun "LinkW" :void
  (f :pointer)
  (u :unsigned-int)
  (v :unsigned-int)
  (w :double))

(defcfun "getCost" :double
  (f :pointer)
  (u :unsigned-int)
  (v :unsigned-int))

(defcfun "update" :void
  (f :pointer)
  (v :unsigned-int)
  (w :double))

(defcfun "getMin" :unsigned-int
  (f :pointer)
  (v :unsigned-int))



(defparameter *f* nil)
;;(allocLCT 10)

(loop for line = (read-line)
      while line do
      (when (not (eql #\# (char line 0)))
	(setf op (read-from-string line))
	(setf tok (split-sequence:SPLIT-SEQUENCE #\Space line))
	(cond
	  ((string-equal (car tok) "allocLCT")
	   (when *f* (foreign-funcall "free" :pointer *f*))
	   (setf *f* (allocLCT (read-from-string (cadr tok)))))

	  ((string-equal (car tok) "Access")
	   (Access *f*
		   (read-from-string (cadr tok))
		   (read-from-string (caddr tok))))
	  ((string-equal (car tok) "getNode")

	   )
	  ((string-equal (car tok) "edgeQ")

	   )
	  ((string-equal (car tok) "cut")

	   )
	  ((string-equal (car tok) "reRoot")

	   )
	  ((string-equal (car tok) "LCA")

	   )
	  ((string-equal (car tok) "Link")

	   )
	  ((string-equal (car tok) "LinkW")

	   )
	  ((string-equal (car tok) "getCost")

	   )
	  ((string-equal (car tok) "update")

	   )
	  ((string-equal (car tok) "getMin")

	   )
	  ((string-equal (car tok) "end") (return)))
	(format t "~a~%" (car tok))
	))

;; (link *f* 2 3)

;; (format t "~d~%" (access *f* 2))
;; (format t "~d~%" (access *f* 3))

(foreign-funcall "free" :pointer *f*)
