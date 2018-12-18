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

(defmacro once-only ((&rest names) &body body)
  "A macro-writing utility for evaluating code only once."
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))
(defun prepare (str)
  (concatenate 'string (string #\() str (string #\))))
(defun enclose (char)
  (cond ((char= char #\{) #\})
        ((char= char #\[) #\])
        ((char= char #\() #\))
        ((char= char #\<) #\>)
        (t char)))
(defmacro with-gensyms (symbols &body body)
  "Create gensyms for those symbols."
  `(let (,@(mapcar #'(lambda (sym)
                       `(,sym ',(gensym))) symbols))
     ,@body))
(defun backquote-remove (str)
  "TODO: Make this work better!"
  (remove #\` str))
(defun push-on (elt stack)
  (vector-push-extend elt stack) stack)
(defmacro with-macro-fn (char new-fn &body body)
  (once-only (char new-fn)
    (with-gensyms (old)
      `(let ((,old (get-macro-character ,char)))
         (progn (set-macro-character ,char ,new-fn)
                (prog1 (progn ,@body) (set-macro-character ,char ,old)))))))
(defun cleanup (remove atoms)
  (remove-duplicates (mapcar #'(lambda (x)
                                 (intern (set-difference remove (symbol-name x))))
                             (remove-if-not #'g!-symbol-p atoms))
                     :test #'(lambda (x y)
                               (string= (symbol-name x)
                                        (symbol-name y)))))
(defun read-atoms (str remove)
  "Lisp code in a string to work around SBCL's implementation."
  (cleanup remove
   (with-macro-fn #\, nil
     (flatten (read-from-string (backquote-remove (prepare str)) nil nil)))))
(defun read-to-string (stream terminating-char
                       &optional (acc (make-array 0 :adjustable t :fill-pointer 0)))
  (let ((ch (read-char stream nil nil)))
    (if (and ch (not (char= terminating-char ch)))
        (read-to-string stream terminating-char (push-on ch acc))
        (coerce acc 'string))))
(defmacro with-declarations (form name args body)
  "Preserve docstrings and declarations in autogensyms."
  (with-gensyms (declarations new-body docstring)
    `(multiple-value-bind (,new-body ,declarations ,docstring)
         (parse-body ',body :documentation t)
       `(,',form ,',name ,',args
                 ,(when ,docstring ,docstring)
                 ,@,declarations
                 ,@,new-body))))

(defmacro binds (fn/mac binds &body body)
  "Plural let/let*: Put bindings together. Don't eval the first."
  `(,fn/mac (append ',(car binds) ,@binds) ,@body))
(defmacro lets* (binds &body body)
  `(binds let* ,binds ,@body))
(defmacro lets (binds &body body)
  `(binds let ,binds ,@body))

(defmacro make-autogensym-reader
    ((&optional out-binds* in-binds*) form stream &optional (remove '(#\,)))
  "For functions that read #d, #n, and #g."
  (with-gensyms (str code name syms body in-str in-char in-numarg)
    `(lets* (((,str (prepare (read-to-string ,stream (enclose (read-char ,stream)))))
              (,code (read-from-string ,str nil))
              (,syms (read-atoms ,str ',remove))
              (,body ',code))
             ,binds*)
       (with-declarations ,form (,in-str ,in-char ,in-numarg)
         ,name (let* ,in-binds* ,body)))))
(defun |#d-reader| (stream char numarg)
  (declare (ignore char numarg))
  (make-autogensym-reader ((os (remove-if-not #'o!-symbol-p (remove-duplicates atoms)))
                           (gs (mapcar #'o!-symbol-to-g!-symbol os)))
                          defmacro
                          stream
                          (mapcar #'list gs os)
                          (mapcar (lambda (s)
                                    `(,s (gensym (subseq (symbol-name ,s) 2))))
                                  x!-list)
                          '(#\, #\')))
(defun |#n-reader| (stream char numarg)
  (declare (ignore char numarg))
  (make-autogensym-reader () 'defun stream))
(defun |#g-reader| (stream char numarg)
  (declare (ignore char numarg))
  (make-autogensym-reader () str code syms defmacro stream))
