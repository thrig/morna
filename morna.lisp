; morna - simple "text" pattern generation routines

(defpackage #:morna
  (:use #:cl)
  (:export #:morna-border #:morna-multiply))
(in-package #:morna)

; NOTE the index (0 . 0) is not handled by this macro, hence the plusp
; in the name. 'max' is the limit the indices should not get past
(defmacro with-plusp-indices (indices rollover maximums &body body)
  "Loops over the index sets for a given list of maximum values, but not the first value."
  (let ((len (gensym)) (max-index (gensym)) (index (gensym)))
  `(let* ((,rollover) (,len (list-length ,maximums))
          (,indices (loop repeat ,len collect 0))
          (,max-index (1- ,len)) (,index ,max-index))
     (loop until (minusp ,index) do
       (if (>= (incf (nth ,index ,indices)) (nth ,index ,maximums))
         (progn
           (setf (nth ,index ,indices) 0)
           (setf ,rollover t)
           (decf ,index))
         (progn
           ,@body
           (setf ,index ,max-index)
           (setf ,rollover nil)))))))

(defun morna-border (src width fill &aux (srcdim (array-dimensions src)))
  "Adds a border of the given width and fill element."
  (let* ((dstdim (loop for x in srcdim collect (+ x (* width 2))))
         (dst (make-array dstdim :element-type (array-element-type src)
                                 :initial-element fill)))
    (setf (apply #'aref dst (loop for x in srcdim collect width))
          (row-major-aref src 0))
    (with-plusp-indices srcidx newrow? srcdim
      (setf (apply #'aref dst (loop for x in srcidx collect (+ x width)))
            (apply #'aref src srcidx)))
    dst))

; NOTE 'factors' must be a list of strictly positive integers;
; otherwise, various assumptions like 'dst' being a larger exact
; multiple of 'src' will breakdown. the integers probably should
; be small
(defun morna-multiply (src &rest factors
       &aux (srcdim (array-dimensions src)) (dimlen (length srcdim)) (dstdims))
  "Multiplies a pattern over a larger area."
  (unless (= dimlen (length factors))
    (error "factors length does not match array dimensions (~a, ~a)"
           (length factors) dimlen))
  (setf dstdims (loop for sd in srcdim for fc in factors collect (* sd fc)))
  (let ((dst (make-array dstdims :element-type (array-element-type src)))
        (srcidx 0)
        (srclen (first (last srcdim)))
        (srcmax (array-total-size src))
        (srcoffset 0))
    (setf (row-major-aref dst 0) (row-major-aref src 0))
    (with-plusp-indices idx newrow? dstdims
      (when newrow?
        (when (>= (incf srcoffset srclen) srcmax)
          (setf srcoffset 0)))
      (setf srcidx (mod (1+ srcidx) srclen))
      (setf (apply #'aref dst idx) (row-major-aref src (+ srcoffset srcidx))))
    dst))
