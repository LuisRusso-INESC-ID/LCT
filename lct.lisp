;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload "cffi")

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
	(cond
	  ((eql op "Access")

	   )
	  ((eql op "getNode")

	   )
	  ((eql op "edgeQ")

	   )
	  ((eql op "cut")

	   )
	  ((eql op "reRoot")

	   )
	  ((eql op "LCA")

	   )
	  ((eql op "Link")

	   )
	  ((eql op "LinkW")

	   )
	  ((eql op "getCost")

	   )
	  ((eql op "update")

	   )
	  ((eql op "getMin")

	   )
	  ((eql op end) (return)))
	(format t "~a~%" (read-from-string line))
	))

;; (link *f* 2 3)

;; (format t "~d~%" (access *f* 2))
;; (format t "~d~%" (access *f* 3))

(foreign-funcall "free" :pointer *f*)
