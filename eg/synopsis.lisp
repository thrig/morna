; synopsis.lisp - so that the README example code is not made up and
; therefore is less likely to be buggy

;(ql:quickload :morna)
(require :asdf)
(asdf:load-system :morna)

(defpackage #:synopsis (:use #:cl #:morna))
(in-package #:synopsis)

(defun display (grid)
  (let ((dim (array-dimensions grid)))
    (dotimes (r (first dim))
      (dotimes (c (second dim)) (format t "~c" (aref grid r c)))
      (fresh-line))))

(defun chain-ops (grid ops)
  (dolist (opl ops)
    (let ((fn (pop opl)))
      (setf grid (apply fn grid opl))))
  grid)

(display
  (chain-ops #2A((#\. #\#) (#\# #\.))
    '((morna-multiply 3 7)
      (morna-border 1 #\#)
      (morna-border 1 #\.)
      (morna-border 1 #\#))))
