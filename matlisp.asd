;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10 -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright (c) 2000 The Regents of the University of California.
;;; All rights reserved.
;;;
;;; Permission is hereby granted, without written agreement and without
;;; license or royalty fees, to use, copy, modify, and distribute this
;;; software and its documentation for any purpose, provided that the
;;; above copyright notice and the following two paragraphs appear in all
;;; copies of this software.
;;;
;;; IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
;;; FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
;;; ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
;;; THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;
;;; THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
;;; PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
;;; CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
;;; ENHANCEMENTS, OR MODIFICATIONS.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem :matlisp
  :licence "LLGPL"
  :author "See AUTHORS"
  :homepage "https://github.com/matlisp/"
  :depends-on (#:cffi #+sbcl #:sb-cltl2 #:iterate #:trivia #:trivia.ppcre #:named-readtables #:lambda-reader #:parse-float #:ieee-floats #:yacc #:trivial-garbage #:trivial-types #:closer-mop #:external-program #:bordeaux-threads #:fiveam)
  :components
  ((:module #:src :components
	    ((:file "package")
	     (:file "conditions" :depends-on ("package"))
	     (:module #:utilities :depends-on ("conditions") :components
		      ((:file "functions")
		       (:file "string")
		       (:file "macros" :depends-on ("functions"))
		       (:file "search" :depends-on ("macros" "functions"))
		       (:file "dlist" :depends-on ("macros" "functions"))
		       (:file "union-find" :depends-on ("macros" "functions"))
		       (:file "lvec" :depends-on ("macros" "functions"))
		       (:file "template" :depends-on ("macros" "functions"))))
	     (:module #:core :pathname "" :depends-on (#:utilities) :components
		      ((:module #:ffi :components
				((:file "foreign-vector")
				 (:file "cffi")
				 (:file "ffi")))
		       (:module #:base :components
				((:file "variables")
				 (:file "base-tensor" :depends-on ("variables"))
				 (:module #:template :pathname "" :depends-on ("base-tensor") :components
					  ((:file "loopy") (:file "generator") (:file "numeric-template")
					   (:file "tensor-template" :depends-on ("generator" "numeric-template"))))
				 (:file "generic" :depends-on (#:template))
				 (:file "print" :depends-on (#:generic))
				 (:module #:accessor :pathname "" :depends-on (#:template) :components
					  ((:file "stride-accessor")
					   (:file "graph-accessor")
					   (:file "coordinate-accessor")))
				 (:file "permutation" :depends-on (#:generic))
				 (:file "blas-helpers" :depends-on (#:accessor))
				 (:file "einstein" :depends-on (#:template #:accessor))
				 (:file "slice" :depends-on (#:template #:accessor))
				 (:file "foreign" :depends-on (#:template #:accessor))
				 (:file "boolean" :depends-on (#:template #:accessor))))
		       (:module #:blas :depends-on (#:base) :components
				((:file "maker")
				 (:module #:level-1 :pathname "" :depends-on ("maker") :components
					  ((:file "copy")	(:file "dot")
					   (:file "axpy" :depends-on ("copy"))
					   (:file "scal" :depends-on ("copy"))
					   (:file "realimag" :depends-on ("copy"))
					   (:file "transpose" :depends-on ("scal" "copy"))
					   (:file "sum" :depends-on ("dot" "copy"))))
				 (:file "gem" :depends-on (#:level-1))
				 (:file "ger" :depends-on (#:level-1))
				 (:file "trs")))
		       (:module #:lapack :depends-on (#:blas) :components
				((:file "lu") (:file "chol")
				 (:file "qr") (:file "least-squares")
				 (:file "eig") (:file "schur")
				 (:file "svd") (:file "syl" :depends-on ("schur"))))))
	     (:module #:graph :depends-on (#:core)
		      :components ((:file "fibonacci") (:file "dfs")
				   (:file "graph" :depends-on ("dfs" "fibonacci"))
				   (:file "graphviz" :depends-on ("dfs" "fibonacci"))))
	     (:module #:special :depends-on (#:core) :components
		      ((:file "random") (:file "map")
		       (:file "norm") (:file "misc")
		       (:file "poly") (:file "optimize")))
	     (:module #:user :depends-on (#:core) :components
		      ((:file "arithmetic") (:file "function")))
	     (:module #:reader :depends-on (#:user) :components
		      ((:file "infix")
		       #+nil (:file "loadsave")))
	     #+weyl
	     (:module #:symbolic :depends-on (#:user) :components
		      ((:file "symbolic")))))))

(asdf:defsystem :matlisp-tests
  :licence "LLGPL"
  :author "See AUTHORS"
  :homepage "https://github.com/matlisp/"
  :depends-on (:matlisp)
  :components
  ((:module #:t :components
	    ((:file "utilities")
	     (:file "lapack")))))
