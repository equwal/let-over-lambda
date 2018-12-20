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
(defvar enclose
  (make-encloses #\{ #\}
                 #\[ #\]
                 #\( #\)
                 #\< #\>))
(defun make-encloses (&rest chars)
  (loop for p in (group chars 2)
        collect (cons (first p) (second p))))
(defun enclose (char)
  (cdr (assoc char enclose)))
(defun listify (str)
  (concatenate 'string (string #\() str (string #\))))
(defmacro with-gensyms (symbols &body body)
  "Create gensyms for those symbols."
  `(let (,@(mapcar #'(lambda (sym)
                       `(,sym ',(gensym))) symbols))
     ,@body))
(defun backquote-remove (str)
  "TODO: Make this work better!"
  (remove #\` str))
(defmacro with-macro-fn (char new-fn &body body)
  (once-only (char new-fn)
    (with-gensyms (old)
      `(let ((,old (get-macro-character ,char)))
         (progn (set-macro-character ,char ,new-fn)
                (prog1 (progn ,@body) (set-macro-character ,char ,old)))))))
(defun cleanup (remove atoms)
  (remove-duplicates (mapcar #'(lambda (x)
                                 (intern (remove remove (symbol-name x))))
                             (remove-if-not #'g!-symbol-p atoms))
                     :test #'(lambda (x y)
                               (string= (symbol-name x)
                                        (symbol-name y)))))
(defun read-atoms (str remove)
  "Lisp code in a string to work around SBCL's implementation."
  (cleanup remove
           (with-macro-fn #\, nil
             (flatten (read-from-string (backquote-remove (listify str)) nil nil)))))
(defun read-to-string (stream &optional (acc (make-array 0 :adjustable t :fill-pointer 0)))
  "Only for delimited strings like #d{...} since it t"
  (if (= 0 (length acc))
      (read-char stream)
      (let ((ch (read-char stream nil nil))
            (en (enclose terminating-char)))
        (if (and ch (not (char= en ch)))
            (read-to-string stream en (push-on ch acc))
            (coerce acc 'string)))))
(defmacro with-declarations (form name args body)
  "Preserve docstrings and declarations in autogensyms."
  (with-gensyms (declarations new-body docstring)
    `(multiple-value-bind (,new-body ,declarations ,docstring)
         (parse-body ',body :documentation t)
       `(,',form ,',name ,',args
                 ,(when ,docstring ,docstring)
                 ,@,declarations
                 ,@,new-body))))
(defmacro env ((syms body) stream remove)
  "The environment required before any and all autogensymming."
  (with-gensyms (str)
    `((,str (listify (read-to-string ,stream (read-char ,stream))))
      (,body (read-from-string ,str nil))
      (,syms (read-atoms ,str ',remove)))))
(defmacro make-autogensym-reader ((&optional binds-out binds-in) form stream
                                  &optional syms (remove '(#\,)))
  "For functions that read #d, #n, and #g."
  (with-gensyms (name body in-str in-char in-numarg)
    `(binds let* `(',(env (,syms ,body) ,stream ,remove)
                                 ,,binds-out)
       (with-declarations ,form ,name (,in-str ,in-char ,in-numarg)
         (binds let (,syms ,binds-in),@body)))))
(defun |#d-reader| (stream char numarg)
  (declare (ignore char numarg))
  (make-autogensym-reader (rest ((os (remove-if-not #'o!-symbol-p (remove-duplicates syms)))
                                 (gs (mapcar #'o!-symbol-to-g!-symbol os)))
                                (mapcar #'list gs os))
                          defmacro
                          stream
                          (mapcar (lambda (s)
                                    `(,s (gensym (subseq (symbol-name ,s) 2))))
                                  x!-list)
                          syms
                          '(#\, #\')))
(defun |#n-reader| (stream char numarg)
  (declare (ignore char numarg))
  (make-autogensym-reader () defun stream))
(defun |#g-reader| (stream char numarg)
  (declare (ignore char numarg))
  (make-autogensym-reader () defmacro stream))
