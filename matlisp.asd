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

(in-package #:common-lisp-user)
(defpackage #:matlisp-system (:use #:cl :asdf))
(in-package #:matlisp-system)

(asdf:defsystem :matlisp-basic
  :depends-on (#:cffi #:iterate #:trivia #:trivia.ppcre #:named-readtables #:lambda-reader #:yacc #:trivial-garbage #:closer-mop #:external-program #:bordeaux-threads #:fiveam)
  :pathname "src"
  :components
  ((:file "packages")
   (:file "conditions" :depends-on ("packages"))
   (:module "utilities"
	    :pathname "utilities" :depends-on ("conditions")
	    :components ((:file "functions")
			 (:file "string")
			 (:file "macros" :depends-on ("functions"))
			 (:file "search" :depends-on ("macros" "functions"))
			 (:file "dlist" :depends-on ("macros" "functions"))
			 (:file "union-find" :depends-on ("macros" "functions"))
			 (:file "lvec" :depends-on ("macros" "functions"))
			 (:file "template" :depends-on ("macros" "functions"))
			 (:file "destructuring" :depends-on ("macros" "functions"))))
   (:file "lazy-loader" :depends-on ("utilities"))))

(asdf:defsystem :matlisp
  :licence "LLGPL"
  :author "See AUTHORS"
  :homepage "https://github.com/matlisp/"
  :depends-on ("matlisp-basic") :pathname "src"
  :components
  ((:module "matlisp-core" :pathname ""
	    :components
	    ((:module "foreign-interface"
		       :pathname "ffi"
		       :components ((:file "foreign-vector")
				    (:file "cffi")
				    (:file "ffi")
				    #+nil(:file "f77-parser")))
	     (:module "matlisp-base" :pathname "base"
		      :components ((:file "tweakable")
				   (:file "base-tensor" :depends-on ("tweakable"))
				   (:file "loopy" :depends-on ("base-tensor"))
				   (:file "generator" :depends-on ("base-tensor"))
				   ;;
				   (:file "numeric-template")
				   (:file "tensor-template" :depends-on ("base-tensor" "generator" "numeric-template"))
				   ;;
				   (:file "generic/copy" :depends-on ("base-tensor" "loopy" "tensor-template"))
				   (:file "generic/ref" :depends-on ("base-tensor"  "loopy" "tensor-template"))
				   (:file "print" :depends-on ("base-tensor" "generic/ref"))
				   ;;
				   (:file "stride-accessor" :depends-on ("tensor-template"))
				   (:file "graph-accessor" :depends-on ("tensor-template"))
				   (:file "coordinate-accessor" :depends-on ("tensor-template"))
				   ;;
				   (:file "permutation" :depends-on ("base-tensor" "generic/copy"))
				   ;;
				   (:file "blas-helpers" :depends-on ("base-tensor" "stride-accessor" "permutation"))
				   (:file "einstein" :depends-on ("base-tensor" "tensor-template" "stride-accessor"))
				   (:file "slice" :depends-on ("base-tensor" "tensor-template" "stride-accessor"))
				   (:file "foreign" :depends-on ("base-tensor" "tensor-template" "stride-accessor"))
				   (:file "boolean" :depends-on ("base-tensor" "tensor-template" "stride-accessor"))))
	     (:module "matlisp-blas" :pathname "blas"
		      :depends-on ("matlisp-base")
		      :components ((:file "maker")
				   (:file "copy" :depends-on ("maker"))
				   (:file "dot" :depends-on ("maker"))
				   (:file "axpy" :depends-on ("maker" "copy"))
				   (:file "scal" :depends-on ("maker" "copy"))
				   (:file "realimag" :depends-on ("copy"))
				   (:file "transpose" :depends-on ("scal" "copy"))
				   (:file "sum" :depends-on ("dot" "copy"))
				   (:file "gem" :depends-on ("copy"))
				   (:file "ger" :depends-on ("copy"))
				   (:file "trs")))
	     (:module "matlisp-lapack" :pathname "lapack"
		      :depends-on ("matlisp-base" "matlisp-blas")
		      :components ((:file "lu")
				   (:file "chol")
				   (:file "eig")
				   (:file "least-squares")
				   (:file "qr")
				   (:file "schur")
				   (:file "svd")
				   (:file "syl" :depends-on ("schur"))))))
   (:module "matlisp-graph" :pathname "graph"
	    :depends-on ("matlisp-core")
	    :components ((:file "fibonacci")
			 (:file "dfs")
			 (:file "graph" :depends-on ("dfs" "fibonacci"))
			 (:file "graphviz" :depends-on ("dfs" "fibonacci"))))
   (:module "matlisp-special" :pathname "special"
	    :depends-on ("matlisp-core")
	    :components ((:file "random")
			 (:file "map")
			 (:file "norm")
			 (:file "misc")))
   ;;Matlisp-user
   (:module "matlisp-user" :pathname "user"
	    :depends-on ("matlisp-core")
	    :components ((:file "arithmetic")
			 (:file "function")))
   (:module "matlisp-reader" :pathname "reader"
	    :depends-on ("matlisp-core" "matlisp-user")
	    :components ((:file "infix")
			 #+nil
			 (:file "loadsave")))
   #+weyl
   (:module "matlisp-symbolic" :pathname "symbolic"
	    :depends-on ("matlisp-user")
	    :components ((:file "symbolic")))
   ))
