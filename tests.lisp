; tests for morna. see "TESTING" in the README or the `runtests` script.
; maybe also run:
;
;   ecl --shell eg/synopsis.lisp
;   sbcl --script eg/synopsis.lisp
;
; FiveAM expects for failure reports the form:
;
;   (equality expected-result test-code)

(defpackage :morna/test (:use #:cl #:morna #:5am))
(in-package :morna/test)

(def-suite :morna-suite)
(in-suite :morna-suite)

(defun display (grid)
  (declare (type (array t (* *)) grid))
  (fresh-line)
  (let ((dim (array-dimensions grid)))
    (dotimes (r (first dim))
      (dotimes (c (second dim)) (format t "~a" (aref grid r c)))
      (fresh-line))))

(test with-plusp-indices
 (let ((idx '(0 0)) (max '(2 3)) out rowchanges)
   (morna::with-plusp-indices (idx newrow? max)
     (push (cons (first idx) (second idx)) out)
     (push newrow? rowchanges))
   (is (equal '((0 . 1) (0 . 2) (1 . 0) (1 . 1) (1 . 2)) (nreverse out)))
   (is (equal '(nil nil t nil nil) (nreverse rowchanges))))
 (let ((idx '(0 0 0)) (max '(2 2 2)) out rowchanges)
   (morna::with-plusp-indices (idx newrow? max)
     (push (copy-list idx) out)
     (push newrow? rowchanges))
   (is (equal '(nil t nil t nil t nil) (nreverse rowchanges)))
   (is
    (equal '((0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))
           (nreverse out)))))

(test morna-border
 (is (equalp #(#\A #\B #\B #\A) (morna-border #(#\B #\B) 1 #\A)))
 (is (equalp #(4 4 4 5 4 4 4) (morna-border #(5) 3 4)))
 (is
  (equalp
   #3A(((1 1 1) (1 1 1) (1 1 1))
       ((1 1 1) (1 5 1) (1 1 1))
       ((1 1 1) (1 1 1) (1 1 1)))
   (morna-border #3A(((5))) 1 1))))

; TODO needs more bounds checking, see the comments in the code
(test morna-copy!
 (is (equalp #(t nil) (morna-copy! #(nil nil) #(t))))
 (is (equalp #(nil t) (morna-copy! #(nil nil) #(t) '(1))))
 (is
  (equalp #2A((nil nil nil nil) (nil nil t t))
          (morna-copy! #2A((nil nil nil nil) (nil nil nil nil)) #2A((t t))
           '(1 2)))))

(test morna-crop
 (is (equalp #(2 7 3) (morna-crop #(1 2 7 3) '(1) '(0))))
 (is (equalp #(7) (morna-crop #(1 2 7 3) '(2) '(1))))
 (is
  (equalp #3A(((5)))
          (morna-crop
           #3A(((1 1 1) (1 1 1) (1 1 1))
               ((1 1 1) (1 5 1) (1 1 1))
               ((1 1 1) (1 1 1) (1 1 1)))
           '(1 1 1) '(1 1 1)))))

; zero-dimensional arrays don't make much sense for this code but are
; tested to ensure nothing bad happens if one shows up
(test morna-flip-cols!
 (is (equalp #() (morna-flip-cols! #())))
 (is (equalp #(9) (morna-flip-cols! #(9))))
 (is (equalp #(7 6 5) (morna-flip-cols! #(5 6 7))))
 (is
  (equalp #2A((4 3 2 1) (5 4 3 2) (7 6 5 4))
          (morna-flip-cols! #2A((1 2 3 4) (2 3 4 5) (4 5 6 7))))))

(test morna-flip-rows!
 (is (equalp #2A((4 5 6) (1 2 3)) (morna-flip-rows! #2A((1 2 3) (4 5 6)))))
 (is
  (equalp #2A((7 8 9) (4 5 6) (1 2 3))
          (morna-flip-rows! #2A((1 2 3) (4 5 6) (7 8 9))))))

(test morna-mask!
 (is (equalp #(1 8 7 4 5 6) (morna-mask! #(1 0 0 4 0 6) 0 #(9 8 7 6 5 4))))
 )

(test morna-multiply
 (is (equalp #(a b a b) (morna-multiply #(a b) 2)))
 (is (equalp #(a b a b a b) (morna-multiply #(a b) 3)))
 (is (equalp #2A((a b c) (d e f)) (morna-multiply #2A((a b c) (d e f)) 1 1)))
 (is
  (equalp
   #2A((a b c a b c)
       (d e f d e f)
       (a b c a b c)
       (d e f d e f)
       (a b c a b c)
       (d e f d e f))
   (morna-multiply #2A((a b c) (d e f)) 3 2)))
 (is
  (equalp
   #2A((a b c a b c a b c)
       (d e f d e f d e f)
       (a b c a b c a b c)
       (d e f d e f d e f))
   (morna-multiply #2A((a b c) (d e f)) 2 3)))
 (is
  (equalp
   #3A(((a b a b) (c d c d) (e f e f) (g h g h))
       ((a b a b) (c d c d) (e f e f) (g h g h))
       ((a b a b) (c d c d) (e f e f) (g h g h))
       ((a b a b) (c d c d) (e f e f) (g h g h)))
   (morna-multiply #3A(((a b) (c d)) ((e f) (g h))) 2 2 2))))
; (let* ((input
;         (make-array '(3 3) :element-type 'standard-char :initial-contents
;                     '((#\. #\# #\.) (#\. #\. #\#) (#\# #\# #\#))))
;        (output (morna-multiply input 3 3)))
;   (format t "~%~a" output)))

; this may chew up some memory, but smaller arrays may show too much
; variance from too few points. there may be test failures should the
; RNG go on a rare streak that hopefully the array size ameliorates
(test morna-noise!
 (let* ((yesno
         (make-array '(2 10 10) :element-type 'boolean :initial-element nil))
        (size (array-total-size yesno))
        (count 0))
   (morna-noise! yesno t 0.5)
   (loop for x from 0 below size do
     (when (row-major-aref yesno x) (incf count)))
   (is (string= "0.5" (format nil "~,1f" (/ count size)))))
 (let* ((nums
         (make-array '(10 10 10) :element-type 'fixnum :initial-element 0))
        (size (array-total-size nums))
        (counta 0) (countb 0))
   (morna-noise! nums (lambda (x y) (if (< (random 1.0) 0.5) 1 2)) 0.5)
   (loop for x from 0 below size do
     (ecase (row-major-aref nums x)
       (2 (incf countb))
       (1 (incf counta))
       (0)))
   (is (string= "0.25" (format nil "~,2f" (/ counta size))))
   (is (string= "0.25" (format nil "~,2f" (/ countb size))))))

(test morna-rotate-grid
 (let ((six #2A((4 5 6) (1 2 3))))
   (is (equalp #2A((6 3) (5 2) (4 1)) (morna-rotate-grid six :90)))
   (is (equalp #2A((3 2 1) (6 5 4)) (morna-rotate-grid six :180)))
   (is (equalp #2A((1 4) (2 5) (3 6)) (morna-rotate-grid six :270)))))

(test morna-trim
 (is (equalp #(7) (morna-trim #(1 7 2) 1)))
 (is
  (equalp #2A((7))
          (morna-trim
           #2A((1 1 1 1 1) (2 2 2 2 2) (3 3 7 3 3) (2 2 2 2 2) (1 1 1 1 1))
           2))))
