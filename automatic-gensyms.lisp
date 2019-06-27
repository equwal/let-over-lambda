;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: LET-OVER-LAMBDA; Base: 10 -*- file: let-over-lambda.lisp

;;;; This file for automatic gensym reader macros on SBCL, since SBCL doesn't
;;;; support it otherwise.

;; Antiweb (C) Doug Hoyte

;; This is a "production" version of LOL with bug-fixes
;; and new features in the spirit of the book.

;; See http://letoverlambda.com

;; This is the source code for the book
;; _Let_Over_Lambda_ by Doug Hoyte.
;; This code is (C) 2002-2008, Doug Hoyte.
;;
;; You are free to use, modify, and re-distribute
;; this code however you want, except that any
;; modifications must be clearly indicated before
;; re-distribution. There is no warranty,
;; expressed nor implied.
;;
;; Attribution of this code to me, Doug Hoyte, is
;; appreciated but not necessary. If you find the
;; code useful, or would like documentation,
;; please consider buying the book!

;; Modifications by "the Phoeron" Colin J.E. Lupton, 2012--2014
;; - Support for ASDF/Quicklisp

;; Modifications by "Equwal" Spenser Truex 2018-11-17
;; - Reader macros for defmacro/g!, defmacro!, and defun! since SBCL won't
;;   allow otherwise.

;;;; This file for automatic gensym reader macros on SBCL, since SBCL doesn't
;;;; support it otherwise.

;; Antiweb (C) Doug Hoyte

;; This is a "production" version of LOL with bug-fixes
;; and new features in the spirit of the book.

;; See http://letoverlambda.com

;; This is the source code for the book
;; _Let_Over_Lambda_ by Doug Hoyte.
;; This code is (C) 2002-2008, Doug Hoyte.
;;
;; You are free to use, modify, and re-distribute
;; this code however you want, except that any
;; modifications must be clearly indicated before
;; re-distribution. There is no warranty,
;; expressed nor implied.
;;
;; Attribution of this code to me, Doug Hoyte, is
;; appreciated but not necessary. If you find the
;; code useful, or would like documentation,
;; please consider buying the book!

;; Modifications by "the Phoeron" Colin J.E. Lupton, 2012--2014
;; - Support for ASDF/Quicklisp
;; - Cheap hacks to support new Backquote implementation in SBCL v1.2.2

;; Modifications by "equwal" Spenser Truex 2018 (http://truex.eu)
;; - Reader macros for defmacro/g! and defmacro! since SBCL won't allow
;;   otherwise.

;;;; Define reader macros to sidestep SBCL's implementation of backquote.
(in-package #:let-over-lambda)
(defassoc enclose
  #\{ #\}
  #\[ #\]
  #\( #\)
  #\< #\>)
(defun listify (str)
  (concatenate 'string (string #\() str (string #\))))
(defun backquote-remove (str)
  "Just stupidly remove backquotes frome the code."
  (remove #\` str))
(defmacro with-macro-fn (char new-fn &body body)
  "Change reader macro inside this body of code."
  (once-only (char new-fn)
    (with-gensyms (old)
      `(let ((,old (get-macro-character ,char)))
         (progn (set-macro-character ,char ,new-fn)
                (prog1 (progn ,@body)
                  (set-macro-character ,char ,old)))))))
(defun symlist/g! (remove atoms)
  (remove-duplicates (mapcar #'(lambda (x)
                                 (intern (remove remove (symbol-name x))))
                             (remove-if-not #'g!-symbol-p atoms))
                     :test #'(lambda (x y)
                               (string= (symbol-name x)
                                        (symbol-name y)))))
(defun read-atoms (str remove)
  "Lisp code in a string to work around SBCL's implementation."
  (symlist/g! remove
              (with-macro-fn #\, nil
                (flatten (read-from-string (backquote-remove (listify str)) nil nil)))))
(defun read-to-string (stream)
  "For delimited reader macros like #d{}, ends on the next }."
  (labels ((rec (en acc)
             (let ((ch (read-char stream nil nil)))
               (if (and ch (not (char= en ch)))
                   (rec en (push-on ch acc))
                   (coerce acc 'string)))))
    (let ((en (read-char stream)))
      (rec (if (enclose en) (enclose en) en)
           (make-array 0 :adjustable t :fill-pointer 0)))))
(defmacro with-declarations (form name args body)
  "Preserve docstrings and declarations in defmacro and defun."
  (with-gensyms (declarations new-body docstring)
    `(multiple-value-bind (,new-body ,declarations ,docstring)
         (parse-body ,body :documentation t)
       `(,',form ,',name ,',args
                 ,(when ,docstring ,docstring)
                 ,@,declarations
                 ,@,new-body))))
(defun defmacro!-reader (stream)
  (let* ((str (read-to-string stream))
         (code (read-from-string str))
         (syms (read-atoms str '(#\,)))
         (os (remove-if-not #'o!-symbol-p syms))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    (let ((body (cddr code)))
      `(with-declarations defmacro ,(car code) ,(cadr code)
         (body declarations docstring)
         (let ,(mapcar
                (lambda (s)
                  `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2))))
                syms)
           `(let ,(mapcar #'list (list ,@gs) (list ,@os))
              ,,@body))))))
(defun make-autogensym-reader (form stream)
  (let* ((str (read-to-string stream))
         (code (read-from-string str))
         (syms (read-atoms str '(#\,))))
    (let ((body (cddr code)))
      `(with-declarations ,form ,(car code) ,(cadr code)
         (body declarations docstring)
         (let ,(mapcar
                (lambda (s)
                  `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2))))
                syms)
           ,@body)))))
(defun |#d-reader| (stream char numarg)
  (declare (ignore char numarg))
  (defmacro!-reader stream))
(defun |#n-reader| (stream char numarg)
  (declare (ignore char numarg))
  (make-autogensym-reader 'defun stream))
(defun |#g-reader| (stream char numarg)
  (declare (ignore char numarg))
  (make-autogensym-reader 'defmacro stream))
