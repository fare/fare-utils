;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-

#+xcvb (module nil)

(in-package #:cl-user)

#|;; After a mismatch warning, regenerate with
(write (fare-utils:make-defpackage-form :fare-utils :gensym) :pretty t :case :downcase)
(terpri)
|#

(defpackage #:fare-utils
  (:use #:common-lisp #:asdf-utils)
  #+genera (:import-from #:scl #:boolean)
  ;; #+clisp (:shadow :with-gensyms)
  (:export
   #:$buffer-size #:*package-misdefinition-warning-hook*
   #:*safe-package* #:*standard-readtable*
   #:+all-chars-base-feature+ ;; #:absolute-pathname-p
   #:accessors-equal-p #:acond #:acond2 #:adjust-size #:aif
   #:aif2 #:alist->hash-table #:association
   #:base-char-p #:binary-heap #:binomial-heap #:boolean
   #:cached-size-mixin
   #:call-with-input-stream #:call-with-output-stream
   #:check-not-empty
   #:clobber-file-if-different #:clobber-file-with-vector
   #:combinationp #:conc-gensym ;; #:multiple-value-compose #:multiple-value-compose/2
   #:conc-keyword #:conc-string #:conc-symbol #:conc-symbol-in
   #:cond2 #:cons-tree-map ;; #:copy-array #:copy-array-shape
   #:copy-list-without-nth
   #:copy-symbol-function #:copy-symbol-value #:dbg #:dbg-time
   #:declaim-type #:declare-type
   #:define-package-mix
   #:def*class #:def*constant
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
   #:featurify ;; #:extremum
   #:file-contents-equal-vector-p #:fill-array #:find-item
   #:find-least-item #:find-node #:first-and-only #:firstn
   #:get-file-contents
   #:hash-table->alist #:hashmacro #:hashmacros #:heap #:if-bind #:when-bind
   #:if-testing #:if2 #:initialize-instance #:insert-item!
   #:insert-node! #:integers-below #:integers-between #:it
   #:join-strings #:kwote #:least-item #:length<-p #:length<=-p
   #:length<=n-p #:length<n-p #:length=-p ;; #:length=n-p
   #:length>-p #:length>=-p #:length>=n-p #:length>n-p #:let1
   #:list->vector #:list-of-integers
   #:literalp #:make-collector #:make-defpackage-form
   #:make-node #:make-predicate-symbol
   #:make-single-arg-form #:mapcar2 #:mapmacro ;; #:mappend #:append/list
   #:maybe-adjust-size-down #:maybe-adjust-size-up
   #:msg #:multiple-value-quote #:mvbind
   #:mvcall #:mvlist #:mvprog1 #:mvquote #:mvsetq
   #:ndolist-with-rest #:ndolist-with-rest-fun #:nilf #:niy
   #:node-item #:node-mixin #:nop
   #:null-string-p #:plist->alist #:pop-last-item
   #:pop-last-item! #:pop-least-item #:pop-least-item! #:pop-item!
   #:post-decf #:post-incf #:propmacro #:propmacros
   #:prune-node! #:push-last-item! #:quotep #:rcons
   #:remove-nth #:rlist* #:safe-read #:safe-write #:search-tree
   #:set-container-contents-from-list! #:simplify-string
   #:single-arg #:single-arg-form-p #:sized-container-mixin
   #:string-all-base-char-p #:string-basic-p ;; #:strcat
   #:->string #:style-warn #:test-form #:test-forms
   #:test-only #:the* #:ttest #:ttest* #:unfeaturify
   #:vector->list #:vector-container-mixin
   #:vector-container-ref #:with-buffered-file-contents
   #:with-input #:with-magic-special-variables ;; #:with-gensyms
   #:with-magic-special-variables-safely #:with-msv #:with-msv*
   #:with-output-stream #:with-input-stream
   #:with-user-output-file #:xtime #:_
   #:but-last-char ;; #:first-char #:last-char
   ;; #:proper-list-p
   #:form-starting-with-p
   #:make-hashset
   #:split-list
   #:string-prefix-p #:string-suffix-p #:string-enclosed-p
   #:string-strip-prefix #:string-strip-suffix
   #:+root-path+ #:+back-path+ ;; #:merge-pathnames*
   ;; #:pathname-directory-pathname #:pathname-base-pathname
   ;; #:pathname-parent
   #:top-level-name #:directory-name-p
   #:portable-pathname-string-component-char-p
   #:portable-pathname-string-component-p
   #:portable-pathname-type-component-p
   #:portable-pathname-directory-output
   #:portable-pathname-name-output
   #:portable-pathname-type-output
   #:portable-pathname-output
   #:portable-namestring
   #:portable-pathname-from-string
   ;; #:subpathname
   #:pathname-absolute-p
   #:portable-namestring-absolute-p
   #:portable-pathname-absolute-p
   #:absolute-portable-namestring-p
   #:portable-namestring-p
   ;;#:ensure-absolute-pathname ;; use ASDF:ENSURE-PATHNAME-ABSOLUTE
   #:portable-namestring-prefix<=
   ;;#:ensure-pathname-is-directory
   #:rename-file-overwriting-target
   #:unwilden
   #:append1 #:append1f #:funcallf ;; #:appendf #:nconcf
   #:with-nesting #:nest #:tsen
   ;; #:while-collecting
   #:parse-macro-lambda-list
   #:list-starts-with-p
   ;; #:parse-body
   #:parse-defsetf-lambda-list
   #:identifierp
   #:package-functions
   #:trace-package-functions
   #:untrace-package-functions
   #:vector-bind
   ))
