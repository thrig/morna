#|
eval 'exec sbcl --script "$0" ${1+"$@"}'
 # synopsis.lisp - so that the README example code is not made up and
 # therefore is less likely to be buggy. assumes ASDF install of m.
|#

;(ql:quickload :morna :silent t)
(require :asdf)
(asdf:load-system :morna)

(defpackage #:synopsis (:use #:cl #:morna))
(in-package #:synopsis)

(morna-chain!
  #2A((#\. #\#) (#\# #\.))
  '((morna-multiply 3 7)
    (morna-border 1 #\#)
    (morna-border 1 #\.)
    (morna-border 1 #\#)
    (morna-display-grid)))
