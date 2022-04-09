(defpackage :morna/test (:use #:cl #:morna #:5am))
(in-package :morna/test)

(def-suite :morna-suite)
(in-suite :morna-suite)

(test with-plusp-indices
 (let ((idx '(0 0)) (max '(2 3)) out rowchanges)
   (morna::with-plusp-indices idx newrow? max
    (push (cons (first idx) (second idx)) out)
    (push newrow? rowchanges))
   (is (equal (nreverse out) '((0 . 1) (0 . 2) (1 . 0) (1 . 1) (1 . 2))))
   (is (equal (nreverse rowchanges) '(nil nil t nil nil))))
 (let ((idx '(0 0 0)) (max '(2 2 2)) out rowchanges)
   (morna::with-plusp-indices idx newrow? max
    (push (copy-list idx) out)
    (push newrow? rowchanges))
   (is
    (equal (nreverse out)
           '((0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))))
   (is (equal (nreverse rowchanges) '(nil t nil t nil t nil)))))

(test morna-border
 (is (equalp (morna-border #(#\B #\B) 1 #\A) #(#\A #\B #\B #\A)))
 (is (equalp (morna-border #(5) 3 4) #(4 4 4 5 4 4 4)))
 (is
  (equalp (morna-border #3A(((5))) 1 1)
          #3A(((1 1 1) (1 1 1) (1 1 1))
              ((1 1 1) (1 5 1) (1 1 1))
              ((1 1 1) (1 1 1) (1 1 1))))))

(test morna-multiply
 (is (equalp (morna-multiply #(a b) 2) #(a b a b)))
 (is (equalp (morna-multiply #(a b) 3) #(a b a b a b)))
 (is (equalp (morna-multiply #2A((a b c) (d e f)) 1 1) #2A((a b c) (d e f))))
 (is
  (equalp (morna-multiply #2A((a b c) (d e f)) 3 2)
          #2A((a b c a b c)
              (d e f d e f)
              (a b c a b c)
              (d e f d e f)
              (a b c a b c)
              (d e f d e f))))
 (is
  (equalp (morna-multiply #2A((a b c) (d e f)) 2 3)
          #2A((a b c a b c a b c)
              (d e f d e f d e f)
              (a b c a b c a b c)
              (d e f d e f d e f))))
 ; TODO do some SDL rendering in 3D to better confirm for various
 ; factors that the math is aright. 4D and higher would probably be
 ; harder to visualize?
 (is
  (equalp (morna-multiply #3A(((a b) (c d)) ((e f) (g h))) 2 2 2)
          #3A(((a b a b) (c d c d) (e f e f) (g h g h))
              ((a b a b) (c d c d) (e f e f) (g h g h))
              ((a b a b) (c d c d) (e f e f) (g h g h))
              ((a b a b) (c d c d) (e f e f) (g h g h))))))
; (let* ((input
;         (make-array '(3 3) :element-type 'standard-char :initial-contents
;                     '((#\. #\# #\.) (#\. #\. #\#) (#\# #\# #\#))))
;        (output (morna-multiply input 3 3)))
;   (format t "~%~a" output)))
