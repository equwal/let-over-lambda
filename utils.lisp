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
