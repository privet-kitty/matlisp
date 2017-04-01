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
;;; Define the packages and symbols for Matlisp.

(in-package #:common-lisp-user)

(defpackage "MATLISP-CONDITIONS"
  (:use #:common-lisp)
  (:export
   ;;<conditon {accessors*}>
   ;;Generic errors
   #:generic-error
   #:dimension-mismatch
   #:assumption-violated
   #:invalid-type
   #:invalid-arguments
   #:invalid-value
   #:unknown-token
   #:parser-error
   #:coercion-error
   #:out-of-bounds-error
   #:non-uniform-bounds-error
   ;;Permutation conditions
   #:permutation
   #:permutation-invalid-error
   #:permutation-permute-error
   ;;Tensor conditions
   #:tensor-error
   #:tensor-store-index-out-of-bounds
   #:tensor-insufficient-store
   #:tensor-not-matrix
   #:tensor-not-vector
   #:tensor-index-out-of-bounds
   #:tensor-index-rank-mismatch
   #:tensor-invalid-head-value
   #:tensor-invalid-dimension-value
   #:tensor-invalid-stride-value
   #:tensor-cannot-find-counter-class
   #:tensor-cannot-find-optimization
   #:tensor-dimension-mismatch
   #:tensor-type-mismatch
   #:tensor-store-not-consecutive
   #:tensor-method-does-not-exist
   ;;Matrix conditions
   #:singular-matrix #:matrix-not-pd
))

(defpackage "MATLISP-UTILITIES"
  (:use #:common-lisp #:iterate #:trivia #:trivia.ppcre #:matlisp-conditions #:alexandria)
  (:nicknames #:mu)
  (:export ;;Alexandria
	   #:ensure-list #:curry #:compose #:flatten #:when-let #:if-let #:with-gensyms #:define-constant
	   ;;
	   #:zip #:zipsym #:unzip #:ziptree
	   #:remmeth #:recursive-append
	   #:maptree-if #:maptree #:maptree-eki #:pair
	   #:compile-and-eval #:modproj
	   ;;
	   #:cart #:mapcart  #:infer-type #:make-extensible-array
	   #:cart-case #:cart-ecase #:cart-typecase #:cart-etypecase
	   ;;
	   ;;string
	   #:string+ #:string-join #:file->string #:split-seq #:splitlines
	   ;;lvec
	   #:lvec-foldl #:lvec-foldr #:lvec-max #:lvec-min #:lvec-eq
	   #:lvec-map-foldl! #:lvec-map-foldr! #:lvec-copy
	   #:lvec->list #:lvec->list! #:binary-search #:sort-index
	   ;;Macros
	   #:using-gensyms #:binding-gensyms
	   #:values-n #:rec #:ziprm #:set-slots
	   #:letv* #:let-typed #:let*-typed
	   #:eval-every #:recurse-maadi #:gethash!
	   #:definline #:with-optimization #:very-quickly
	   #:memoizing #:with-memoization))

(defpackage "MATLISP-DLIST"
  (:nicknames #:dlist)
  (:use #:common-lisp #:matlisp-utilities #:iterate)
  (:export #:dpush #:dcons #:dpop #:dlist #:drdc #:dcdr #:dcar #:dappend!))

(defpackage "MATLISP-FIBONACCI"
  (:nicknames :fib)
  (:use #:common-lisp #:matlisp-utilities #:matlisp-dlist #:iterate)
  (:export #:make-heap #:insert-key #:min-key #:node-key #:node-existsp #:extract-min #:decrease-key #:delete-node))

(defpackage "MATLISP-UNION-FIND"
  (:nicknames :ufd)
  (:use #:common-lisp #:matlisp-utilities #:matlisp-dlist #:iterate)
  (:export #:union-find #:insert-item #:find-items #:unite-items #:root #:values #:id))

(defpackage "MATLISP-TEMPLATE"
  (:use #:common-lisp #:iterate #:matlisp-utilities)
  (:export #:deft/generic #:deft/method #:remt/method))

(defpackage "MATLISP-INFIX"
  (:use #:common-lisp #:iterate #:matlisp-conditions #:matlisp-utilities)
  (:import-from :λ-reader #:λ)
  (:export #:test-infix #:string->prefix))

(defpackage "MATLISP-FFI"
  (:use #:common-lisp #:cffi #:iterate #:trivia #:matlisp-utilities #:matlisp-conditions)
  (:export
   ;;Foreign-pointer enclosing structure.
   #:foreign-vector #:element-type #:fvref
   #:ffuncall #:lisp->mffi #:mffi->lisp
   #:with-vector-data-addresses #:vector-sap
   ;;Interface functions
   ;;#:def-fortran-routine #:parse-fortran-file
   )
  (:documentation "Fortran foreign function interface"))

(defpackage "MATLISP"
  (:use #:common-lisp #:iterate #:trivia #:trivia.ppcre #:named-readtables #:matlisp-conditions #:matlisp-utilities #:matlisp-ffi #:matlisp-template)
  (:import-from :λ-reader #:λ)
  (:nicknames :m)
  (:export
   ;;Tweakable
   #:*sparse-tensor-realloc-on-setf* #:*default-sparse-store-increment* #:*default-sparsity* #:*max-sparse-size*
   #:*default-stride-ordering* #:*default-tensor-type*
   #:*check-after-initializing?* #:*rcond-scale* #:*default-uplo*
   #:*real-l1-fcall-lb* #:*real-l2-fcall-lb* #:*real-l3-fcall-lb*
   #:*complex-l1-fcall-lb* #:*complex-l2-fcall-lb* #:*complex-l3-fcall-lb*
   #:+infinity+
   ;;base
   #:dimensions #:order #:field-type #:store-type #:ref #:einstein-sum
   #:base-tensor #:tensor #:memos #:store-size #:total-size #:parent
   #:dorefs #:for-mod #:with-iterator #:loop-order #:uplo
   #:index-type #:index-store-vector

   #:tensor-class #:sparse-tensor #:stride-tensor #:dense-tensor #:simple-dense-tensor #:foreign-dense-tensor
   #:hash-tensor
   #:orphanize #:graph-tensor #:simple-graph-tensor #:coordinate-tensor #:tensor-typep #:tensor-type
   #:tensor-method-generator #:define-tensor-method #:cl
   ;;#:tensor-matrixp #:tensor-vectorp #:tensor-squarep
   #:tensor-vector #:tensor-matrix #:tensor-square-matrix
   #:complexified-tensor #:realified-tensor
   ;;coo, graph
   #:indices #:fence #:neighbors #:δ-I #:strides #:head #:store
   ;;permutation
   #:idxv #:pick-random #:shuffle! #:permutation #:permutation-action #:permutation-cycle
   #:permutation-pivot-flip #:permute! #:permute #:permutation/ #:permutation*
   #:sort-permute #:subtensor~ #:slice~ #:suptensor~ #:reshape! #:reshape~
   #:matrixify~ #:join #:minors
   ;;
   #:zeros #:transpose! #:transpose~ #:transpose #:ctranspose! #:ctranspose
   ;;L1
   #:copy! #:copy #:tricopy! #:swap! #:swap #:axpy! #:axpy #:dot #:scal! #:scal #:div! #:div #:scald!
   #:prod! #:prod #:mean #:normalize!
   ;;Boolean
   #:ge= #:ga= #:go=
   ;;L2, L3
   #:ger! #:ger #:trs! #:gem! #:gem #:gett! #:gekr!
   ;;LAPACK
   #:potrf! #:chol! #:chol #:potrs! #:potri! #:ldl! #:ldl-permute! #:ldl
   #:geev! #:geev-complexify-eigvec  #:heev! #:eig
   #:gelsy #:lstsq
   #:getrf! #:getrs! #:getri! #:lu
   #:qr! #:qr #:schur #:svd #:trsyl! #:syl
   ;;graph
   #:in-graph #:in-order #:with-color #:with-parent #:with-visited-array
   #:graph->adlist #:adlist->graph #:hyper->bipartite #:order->tree #:gnp #:moralize! #:symmetrize! #:graphfib #:max-cardinality-search #:triangulate-graph #:elimination-tree #:cholesky-cover #:chordal-cover #:line-graph #:tree-decomposition #:dijkstra #:dijkstra-prims #:directed-subgraph #:max-dag #:topological-order #:graph->dot #:display-graph
   ;;special
   ;;map
   #:map-tensor! #:map-tensor #:map-tensor! #:mapslice #:mapslice~ #:mapslicec~ #:fold-tensor
   #:for #:slicing #:along #:from #:below #:to #:downto #:with-index #:by
   #:in-vectors #:in-lists #:meshgrid
   #:norm #:psd-proj #:tr #:tensor-max #:tensor-min
   #:ones #:eye! #:eye #:diag #:diaeg~
   #:rand #:randn #:randi #:rande
   #:range #:linspace #:polyfit #:polyval #:roots)
  (:documentation "MATLISP routines"))

;;Shadowed symbols.
(defpackage "MATLISP-USER"
  (:nicknames :t)
  (:use #:common-lisp #:iterate #:matlisp-utilities #:matlisp)
  ;;Shadow iterate:sum
  (:shadow #:+ #:- #:* #:/ #:= #:conjugate #:realpart #:imagpart #:sum #:min #:max
	   #:sqrt #:sin #:cos #:tan #:asin #:acos #:exp #:sinh #:cosh #:tanh #:asinh #:acosh #:atanh #:log #:expt #:atan)
  (:import-from :λ-reader #:λ)
  (:export ;;
   #:realpart~ #:realpart #:imagpart~ #:imagpart #:conjugate! #:conjugate
   #:sum! #:sum
   ;;Boolean
   #:ge= #:ga= #:go=
   ;;arithmetic
   #:+ #:- #:* #:.* #:/ #:./ #:@ #:· #:^ #:⊗ #:= #:.=
   ;;function
   #:sqrt #:sin! #:cos! #:tan! #:asin! #:acos! #:exp! #:sinh! #:cosh! #:tanh! #:asinh! #:acosh! #:atanh!
   #:sin #:cos #:tan #:asin #:acos #:exp #:sinh #:cosh #:tanh #:asinh #:acosh #:atanh
   #:log #:log! #:atan #:atan! #:expt #:expt! #:xlogx #:xlogx!
   ;;map
   #:max #:min)
  (:documentation "MATLISP USER"))

(defpackage "MATLISP-TESTS"
  (:use #:common-lisp #:iterate #:matlisp-utilities #:matlisp #:trivia #:fiveam)
  (:import-from :λ-reader #:λ)
  (:export #:matlisp-tests #:run-tests)
  (:documentation "MATLISP TESTS"))

;;Create a test suite
(fiveam:def-suite matlisp-tests:matlisp-tests
    :description "Regression tests for matlisp-optimization")
(defun matlisp-tests:run-tests () (5am:run! 'matlisp-tests:matlisp-tests))
(fiveam:in-suite matlisp-tests:matlisp-tests)

;;expose matlisp from user
(eval-when (:load-toplevel)
  (do-external-symbols (ss "MATLISP")
    (export ss "MATLISP-USER")))

;;load foreign libs
(eval-when (:load-toplevel)
  (let* ((matlisp-root (asdf:component-pathname (asdf:find-system :matlisp t)))
	 (cffi:*foreign-library-directories*
	  (list* #+linux #P"/usr/lib/"
		 #+darwin #P"/System/Library/Frameworks/Accelerate.framework/Frameworks/vecLib.framework/Versions/A/"
		 cffi:*foreign-library-directories*)))
    (cffi:load-foreign-library `(:or (:default ,(concatenate 'string (namestring matlisp-root) "libblas"))
				     (:default "libblas")
				     (:framework :veclib)))
    (unless (find-if #'cffi:foreign-symbol-pointer '("dgetrf_" "dgetrf__" "DGETRF_" "DGETRF__"))
      (cffi:load-foreign-library `(:or (:default ,(concatenate 'string (namestring matlisp-root) "liblapack"))
				       (:default "liblapack")))))
  #+sbcl
  (setf sb-ext:*inline-expansion-limit* (max 1000 sb-ext:*inline-expansion-limit*)))
