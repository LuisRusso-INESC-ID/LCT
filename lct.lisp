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
  (d :int))

(defcfun "edgeQ" :unsigned-char
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
(loop for line = (read-line)
      for i from 1
      while line do
      (when (not (eql #\# (char line 0)))
	(format t "# ~d : ~a~%" i line)
	(setf tok (split-sequence:SPLIT-SEQUENCE #\Space line))
	(setf op (car tok))
	(cond
	  ((string-equal op "allocLCT")
	   (when *f* (foreign-funcall "free" :pointer *f*))
	   (setf *f* (allocLCT (read-from-string (cadr tok)))))

	  ((string-equal op "Access")
	   (format t "~d~%"
	   (Access *f*
		   (read-from-string (cadr tok)))))
	  ((string-equal op "getNode")
	   (format t "~d~%"
		   (getNode *f*
			    (read-from-string (cadr tok))
			    (read-from-string (caddr tok)))))
	  ((string-equal op "edgeQ")
	   (if (eql 0
		    (edgeQ *f*
			   (read-from-string (cadr tok))
			   (read-from-string (caddr tok))))
	       (format t "False~%")
	       (format t "True~%")))
	  ((string-equal op "cut")
	   (format t "~d~%"
		   (cut *f*
			(read-from-string (cadr tok))
			(read-from-string (caddr tok)))))
	  ((string-equal op "reRoot")
	   (reroot *f*
		   (read-from-string (cadr tok))))
	  ((string-equal op "LCA")
	   (format t "~d~%"
		   (LCA *f*
			(read-from-string (cadr tok))
			(read-from-string (caddr tok)))))
	  ((string-equal op "Link")
	   (Link *f*
		 (read-from-string (cadr tok))
		 (read-from-string (caddr tok))))
	  ((string-equal op "LinkW")
	   (LinkW *f*
		   (read-from-string (cadr tok))
		   (read-from-string (caddr tok))
		   (read-from-string (cadddr tok))))
	  ((string-equal op "getCost")
	   (format t "~6$~%"
		   (getCost *f*
			    (read-from-string (cadr tok))
			    (read-from-string (caddr tok)))))
	  ((string-equal op "update")
	   (update *f*
		   (read-from-string (cadr tok))
		   (read-from-string (caddr tok))))
	  ((string-equal op "getMin")
	   (setf u (getMin *f* (read-from-string (cadr tok))))
	   (format t "~6$~%"
		   (getCost *f* u (getNode *f* u -1))))
	  ((string-equal op "end") (return)))
	))

(foreign-funcall "free" :pointer *f*)
