;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

(in-package #:cl-user)

#|;; After a mismatch warning, regenerate with
(write (fare-utils:make-defpackage-form :fare-utils :gensym) :pretty t :case :downcase)
(terpri)
|#

(defpackage #:fare-utils
  (:use #:common-lisp)
  #+genera (:import-from #:scl #:boolean)
  #+clisp (:shadow :with-gensyms)
  (:export #:$buffer-size #:*package-misdefinition-warning-hook*
           #:*safe-package* #:*standard-readtable*
           #:+all-chars-base-feature+ #:absolute-pathname-p
           #:accessors-equal-p #:acond #:acond2 #:adjust-size #:aif
           #:aif2 #:alist->hash-table #:append/list #:association
           #:base-char-p #:binary-heap #:binomial-heap #:boolean
           #:cached-size-mixin #:call-with-output #:check-not-empty
           #:clobber-file-if-different #:clobber-file-with-vector
           #:combinationp #:compose #:compose/2 #:conc-gensym
           #:conc-keyword #:conc-string #:conc-symbol #:conc-symbol-in
           #:cond2 #:cons-tree-map #:container #:container-add-list!
           #:container-contents #:container-empty #:container-empty-p
           #:container-from-list! #:container-size #:container-sized
           #:containers-empty-p #:copy-array #:copy-array-shape
           #:copy-container #:copy-list-without-nth
           #:copy-symbol-function #:copy-symbol-value #:dbg #:dbg-time
           #:declaim-type #:declare-type #:def*class #:def*constant
           #:def*fun #:def*generic #:def*macro #:def*method
           #:def*package #:def*parameter #:def*setf #:def*struct
           #:def*type #:def*var #:defconstant* #:define*-compiler-macro
           #:define*-condition #:define*-exporter
           #:define*-method-combination #:define*-modify-macro
           #:define*-setf-expander #:define*-symbol-macro
           #:define-abbrevs #:define-enclosing-macro #:define-exporter
           #:define-magic-special-variable #:define-post-modify-macro
           #:define-values-modify-macro
           #:define-values-post-modify-macro #:defmsv #:defsubst
           #:defun-inline #:delete-item! #:delete-node! #:disable-fun
           #:dolist-with-rest #:dolist-with-rest-fun #:empty-container!
           #:enable-fun #:equal-array #:error-behaviour #:eval-now
           #:eval-once #:evaluating-once #:exchange-nodes #:export*
           #:export-symbols #:export-symbols* #:exporting-definitions
           #:extremum #:featurify #:fifo #:fifo-dequeue
           #:fifo-dequeue-object #:fifo-empty-p #:fifo-empty! #:fifo-enqueue
           #:fifo-head #:fifo-nconc2 #:fifo-tail
           #:file-contents-equal-vector-p #:fill-array #:find-item
           #:find-least-item #:find-node #:firstn #:get-file-contents
           #:hash-table->alist #:hashmacro #:hashmacros #:heap #:if-bind
           #:if-testing #:if2 #:initialize-instance #:insert-item!
           #:insert-node! #:integers-below #:integers-between #:it
           #:join-strings #:kwote #:least-item #:length<-p #:length<=-p
           #:length<=n-p #:length<n-p #:length=-p #:length=n-p
           #:length>-p #:length>=-p #:length>=n-p #:length>n-p #:let1
           #:list->vector #:list-from-container! #:list-of-integers
           #:literalp #:make-container-from-list #:make-defpackage-form
           #:make-fifo #:make-node #:make-predicate-symbol
           #:make-single-arg-form #:mapcar2 #:mapmacro #:mappend
           #:maybe-adjust-size-down #:maybe-adjust-size-up
           #:merge-containers! #:msg #:multiple-value-quote #:mvbind
           #:mvcall #:mvlist #:mvprog1 #:mvquote #:mvsetq
           #:ndolist-with-rest #:ndolist-with-rest-fun #:nilf #:niy
           #:node-container-mixin #:node-item #:node-mixin #:nop
           #:null-string-p #:plist->alist #:pop-last-item
           #:pop-last-item! #:pop-least-item #:pop-least-item!
           #:post-decf #:post-incf #:propmacro #:propmacros
           #:prune-node! #:push-last-item! #:quit #:quotep #:rcons
           #:remove-nth #:rlist* #:safe-read #:safe-write #:search-tree
           #:set-container-contents-from-list! #:simplify-string
           #:single-arg #:single-arg-form-p #:sized-container-mixin
           #:strcat #:string-all-base-char-p #:string-basic-p
           #:stuff->string #:style-warn #:test-form #:test-forms
           #:test-only #:the* #:ttest #:ttest* #:unfeaturify
           #:vector->list #:vector-container-mixin
           #:vector-container-ref #:with-buffered-file-contents
           #:with-gensyms #:with-magic-special-variables
           #:with-magic-special-variables-safely #:with-msv #:with-msv*
           #:with-output #:xtime #:_))
