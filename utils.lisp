(in-package #:let-over-lambda)

(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (or (string= (symbol-name s)
                    "G!"
                    :start1 0
                    :end1 2)
           #+sbcl
           (string= (symbol-name s)
                    ",G!"
                    :start1 0
                    :end1 3))))
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))
(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "O!"
                :start1 0
                :end1 2)))
(defun o!-symbol-to-g!-symbol (s)
  (intern (concatenate 'string
                       "G!"
                       (subseq (remove #\, (symbol-name s)) 2))))
(defun group (source n)
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                              (subseq source 0 n)
                              acc))
                   (nreverse
                    (cons source acc))))))
    (if source (rec source nil) nil)))
(defun nthflatten (n list)
  "Reduce nested lists by n levels. Example usage: list of let bindings.
Bonus points: negative n does nthlist wrapping."
  (labels ((flatten-1 (list acc)
             "Reduce a list by one level."
             (if (null list)
                 (nreverse acc)
                 (flatten-1 (cdr list)
                            (loop for b in (car list)
                                  do (push b acc)
                                  finally (return acc))))))
    (cond ((< n 0) (nthflatten (1+ n) (list list)))
          ((= 0 n) list)
          ((> n 0) (nthflatten (1- n) (flatten-1 list nil))))))
(defmacro binds (fn/mac binds &body body)
  "Plural let, let*, etc: Put bindings together."
  `(,fn/mac ,(nthflatten 1 binds)
            ,@body))
(defmacro once-only ((&rest names) &body body)
  "A macro-writing utility for evaluating code only once."
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))
