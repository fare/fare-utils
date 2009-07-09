;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;;;; String utilities

(in-package :fare-utils)

(def*fun join-strings (list &key stream separator)
  (with-output (stream)
    (loop
        with sep = (stuff->string separator)
        for first = t then nil
        for i in list
        unless first do (write-sequence sep stream)
        do (write-sequence i stream))))
