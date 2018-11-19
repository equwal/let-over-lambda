
# LET-OVER-LAMBDA

[![Build Status](https://circleci.com/gh/thephoeron/let-over-lambda.svg?style=shield)](https://circleci.com/gh/thephoeron/let-over-lambda)
[![Build Status](https://travis-ci.org/thephoeron/let-over-lambda.svg?branch=master)](https://travis-ci.org/thephoeron/let-over-lambda)
[![Coverage Status](https://coveralls.io/repos/thephoeron/let-over-lambda/badge.svg?branch=master)](https://coveralls.io/r/thephoeron/let-over-lambda)
[![Quicklisp](http://quickdocs.org/badge/let-over-lambda.svg)](http://quickdocs.org/let-over-lambda/)
[![BSD Simplified License](https://img.shields.io/badge/license-BSD%20Simplified-blue.svg)](./LICENSE)
[![Join the chat at https://gitter.im/thephoeron/let-over-lambda](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/thephoeron/let-over-lambda?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Doug Hoyte's "Production" version of macros from Let Over Lambda, including community updates; available from Quicklisp.

Read more about the book and code at: http://letoverlambda.com

## News &amp; Updates
##### 16/11/2018

Reader macros for `defmacro!` (`#d{...}`), `defmacro/g!` (`#g{...}`), and `defun!` (`#n{}`) added by [Spenser Truex](https://github.com/equwal/). Intended for use only on SBCL which doesn't support them. Here are some example uses:
```
#g{listq (&rest list) 
    (mapcar #'(lambda (,g!x) (list 'quote ,g!x)) ',list)}
#d{square (o!x)
    `(* ,g!x ,g!x)}
#n{gensym () g!x}
```
Any escape character can be used `#d<code>`, `#d|code|`, etc. Watch out though since no other reader macros work inside of the code, incluing strings quoted with `""`. This could be added.


##### 3/19/2015

Add symbols for anaphoric macro internals, `IT`, `THIS`, and `SELF` to package exports for better end-user experience.  Will be available in April 2015 release of Quicklisp.

##### 8/14/2014

Issue with incompatible change to backquote syntax in SBCL 1.2.2 resolved; tested against and builds on SBCL 1.2.0-1 and 1.2.2.  Will be available in the August release of Quicklisp.

##### 12/18/2013

Now available in the December 2013 distribution of Quicklisp

## Usage

Make sure you have the latest Quicklisp distribution, then include it as a dependency in your system definition, or from the REPL evaluate `(ql:quickload "let-over-lambda")`.

```lisp
(ql:quickload "let-over-lambda")
(lol:flatten '((A . B) (C . D) (E . (F G H (I . J) . K))))
=> (A B C D E F G H I J K)
```

LET-OVER-LAMBDA now uses the `named-readtables` library instead of modifying the global readtable. To use LOL reader macros in your Lisp source files, you will have to add both `let-over-lambda` and `named-readtables` to your project dependencies, and the following line after your call to `in-package`, in every source file you wish to use LOL syntax:

```lisp
(named-readtables:in-readtable lol:lol-syntax)`
```

## Contributors

- [Doug Hoyte](https://github.com/hoytech)
- ["the Phoeron" Colin J.E. Lupton](https://github.com/thephoeron)
- [Jorge Gajon](https://github.com/gajon)
- [André Miranda](https://github.com/EuAndreh/)
- [Spenser Truex](https://github.com/equwal/)