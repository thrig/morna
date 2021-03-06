; morna - simple "text" pattern generation routines
;
; there is, in general, a lack of error checking on function inputs
; (e.g. that crop values are strictly positive, or even fixnum) beyond
; what an implementation may complain about regarding type errors or
; missing subscripts. it may help to set the :element-type of arrays
; passed in so that, say, an invalid fill value cannot be added

(defpackage #:morna
  (:use #:cl)
  (:export #:morna-border #:morna-chain! #:morna-clone #:morna-copy!
           #:morna-crop #:morna-display-grid #:morna-flip-both!
           #:morna-flip-cols! #:morna-flip-four #:morna-flip-rows!
           #:morna-fourup #:morna-mask! #:morna-multiply #:morna-noise!
           #:morna-subrect #:morna-rotate-grid #:morna-trim #:morna-truncate
           #:morna-write-file #:morna-upfrac))
(in-package #:morna)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; MACROS AND UTILITY FUNCTIONS

; NOTE the index (0 . 0) is not handled by this macro, hence the plusp
; in the name. 'max' is the limit (or odometer) the indices will not
; exceed (based on "Higher Order Perl" p.131)
(defmacro with-plusp-indices ((indices rollover maximums) &body body)
  "Loops over the index sets for a given list of maximum values, but not the first element."
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

(defun reverse-column! (array offset len)
  "Reverse column values in array starting at offset over range len."
  (declare (fixnum offset len))
  (loop for lo from offset
        for hi from (1- (+ offset len)) downto 0
        while (< lo hi) do
          (rotatef (row-major-aref array lo) (row-major-aref array hi)))
  (values))

; these need not actually be rows; they could be planes from a cube, or
; so forth. hopefully the ranges do not overlap?
(defun swap-rows! (array lo hi size)
  "Swap two blocks of length size in the array."
  (declare (fixnum lo hi size))
  (dotimes (n size)
    (rotatef (row-major-aref array (+ lo n))
             (row-major-aref array (+ hi n)))))

(defmacro with-grid ((grid rows cols) &body body)
  `(destructuring-bind (,rows ,cols) (array-dimensions ,grid) ,@body))

(defmacro with-new-grid ((grid new rows cols) &body body)
  `(let ((,new (make-array `(,,rows ,,cols)
                           :element-type (array-element-type ,grid))))
     ,@body
     ,new))

(defun rotate-90 (grid)
  (with-grid (grid rows cols)
    (with-new-grid (grid new cols rows)
      (loop for sr from 0 below rows do
            (loop for sc from 0 below cols
                  for dc from (1- cols) downto 0 do
                  (setf (aref new dc sr) (aref grid sr sc)))))))

(defun rotate-270 (grid)
  (with-grid (grid rows cols)
    (with-new-grid (grid new cols rows)
      (loop for sr from 0 below rows
            for dr from (1- rows) downto 0 do
            (loop for sc from 0 below cols do
                  (setf (aref new sc dr) (aref grid sr sc)))))))

(defun upfrac-pick (custom-srcs src sources)
  (if custom-srcs
    (etypecase sources
      (list (nth (random (list-length sources)) sources))
      (function (funcall sources)))
    src))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PUBLIC FUNCTIONS

(defun morna-border (src width fill &aux (srcdim (array-dimensions src)))
  "Add a border of the given width and fill element."
  (let* ((dstdim (loop for x in srcdim collect (+ x (* width 2))))
         (dst (make-array dstdim :element-type (array-element-type src)
                                 :initial-element fill)))
    (setf (apply #'aref dst (loop for x in srcdim collect width))
          (row-major-aref src 0))
    (with-plusp-indices (srcidx newrow? srcdim)
      (setf (apply #'aref dst (loop for x in srcidx collect (+ x width)))
            (apply #'aref src srcidx)))
    dst))

(defun morna-chain! (src function-args)
  "Chain operations on src using a list of functions and arguments."
  (dolist (item function-args src)
    (setf src (apply (first item) src (rest item)))))

(defun morna-clone (src &aux (srcdim (array-dimensions src)))
  (let ((dst (make-array srcdim :element-type (array-element-type src))))
    (dotimes (n (array-total-size src))
      (setf (row-major-aref dst n) (row-major-aref src n)))
    dst))

; this breaks with computing tradition and is not called a "move"
(defun morna-copy! (dst src &optional (start-point 0 sp?)
        &aux (dstdim (array-dimensions dst)))
  "Copy from src into dst at the start-point in dst."
  (unless sp?
    (setf start-point (loop repeat (list-length dstdim) collect 0)))
  (setf (apply #'aref dst start-point) (row-major-aref src 0))
  (with-plusp-indices (srcidx newrow? (array-dimensions src))
   (let ((dstidx (loop for x in srcidx for y in start-point collect (+ x y))))
     (when (apply #'array-in-bounds-p dst dstidx)
       (setf (apply #'aref dst dstidx) (apply #'aref src srcidx)))))
  dst)

(defun morna-crop (src croplo crophi)
  "Crop bounded by the crop low and crop high lists."
  (let* ((dstdim (loop for width in (array-dimensions src)
                       for x in croplo for y in crophi
                       collect (- width (+ x y))))
         (dst (make-array dstdim :element-type (array-element-type src))))
    (setf (row-major-aref dst 0) (apply #'aref src croplo))
    (with-plusp-indices (dstidx newrow? dstdim)
      (setf (apply #'aref dst dstidx)
            (apply #'aref src (loop for idx in dstidx for x in croplo
                                    collect (+ idx x)))))
    dst))

(defun morna-display-grid (src &optional (stream t))
  (declare (type (array * (* *)) src))
  (let ((dim (array-dimensions src)))
    (dotimes (r (first dim))
      (dotimes (c (second dim))
        (format stream "~a" (aref src r c)))
      (fresh-line stream)))
  src)

; if rank 0 true, flip columns, rank 1 true flip rows, rank 2 flip
; planes etc. or at least that's how I imagine it going...
;(defun morna-flip! (src ranks)
;  (error "TODO")
;  src)

; also a 180 degree rotation
(defun morna-flip-both! (src &aux (size (array-total-size src)))
  "Flip the rows and columns of src."
  (declare (type (array * (* *)) src))
  (let ((line (make-array size :displaced-to src)))
    (nreverse line))
  src)

; 2D arrays are common so have special cases for ... this one makes
; sense for most all array ranks so is not restricted to 2D
(defun morna-flip-cols! (src)
  "Flip (or mirror) the columns of the array src."
  (do ((column-size (first (last (array-dimensions src))))
       (size (array-total-size src))
       (offset 0 (+ offset column-size)))
    ((>= offset size) src)
    (declare (fixnum offset size)
             (inline reverse-column!))
    (reverse-column! src offset column-size)))

; matches flip_four of Game::TextPatterns, for better or worse
(defun morna-flip-four (src &aux (srcdim (array-dimensions src)))
  (declare (type (array * (* *)) src))
  (let ((cpy (morna-clone src))
        (dst (make-array (mapcar (lambda (x) (* 2 x)) srcdim)
                         :element-type (array-element-type src)))
        (dstidx (copy-list srcdim)))
    ; QI on unit circle
    (setf (nth 0 dstidx) 0)
    (morna-copy! dst cpy dstidx)
    ; QII or [0,0] of dst
    (morna-flip-cols! cpy)
    (morna-copy! dst cpy)
    ; QIII
    (morna-flip-rows! cpy)
    (setf (nth 1 dstidx) 0)
    (setf (nth 0 dstidx) (nth 0 srcdim))
    (morna-copy! dst cpy dstidx)
    ; QIV
    (morna-flip-cols! cpy)
    (setf (nth 1 dstidx) (nth 1 srcdim))
    (morna-copy! dst cpy dstidx)
    dst))

; this could flip only the rows of higher rank arrays, but for now is
; restricted to 2D shapes
(defun morna-flip-rows! (src)
  "Flip (or mirror) the rows of the 2D array src."
  (declare (type (array * (* *)) src))
  (do* ((column-size (first (last (array-dimensions src))))
        (size (array-total-size src))
        (lo 0 (+ lo column-size))
        (hi (- size column-size) (- hi column-size)))
       ((>= lo hi) src)
    (declare (fixnum column-size size lo hi)
             (inline swap-rows!))
    (swap-rows! src lo hi column-size)))

; from four_up of Game::TextPatterns. not much tested
(defun morna-fourup (src fill &aux (srcdim (array-dimensions src)))
  "Rotate src four times into a new grid."
  (declare (type (array * (* *)) src))
  (let* ((hor (morna-clone src))
         (ver (rotate-90 src))
         (dim (* 2 (loop for x in srcdim maximizing x)))
         (dst (make-array `(,dim ,dim)
                          :element-type (array-element-type src)
                          :initial-element fill)))
    ; QI
    (morna-copy! dst hor srcdim)
    ; QII
    (morna-copy! dst ver (list 0 (/ (nth 1 srcdim) 2)))
    ; QIII
    (morna-flip-both! hor)
    (morna-copy! dst hor (list (nth 1 srcdim) 0))
    ; QIV
    (morna-flip-both! ver)
    (morna-copy! dst ver (list (nth 1 srcdim) (nth 1 srcdim)))
    dst))

(defun morna-mask! (dst fill src &key (test #'eql))
  "Insert contents of src into dst where dst contains fill."
  (dotimes (n (min (array-total-size dst) (array-total-size src)))
    (when (funcall test fill (row-major-aref dst n))
      (setf (row-major-aref dst n) (row-major-aref src n))))
  dst)

; NOTE 'factors' must be a list of strictly positive integers;
; otherwise, various assumptions like 'dst' being a larger exact
; multiple of 'src' will breakdown. the integers probably should
; be small
(defun morna-multiply (src &rest factors
       &aux (srcdim (array-dimensions src))
            (dimlen (array-rank src)) (dstdims))
  "Multiply a pattern over a larger area."
  (unless (= dimlen (list-length factors))
    (error "factors length does not match array dimensions (~a, ~a)"
           (list-length factors) dimlen))
  (setf dstdims (loop for sd in srcdim for fc in factors collect (* sd fc)))
  (let ((dst (make-array dstdims :element-type (array-element-type src)))
        (srcidx 0)
        (srclen (first (last srcdim)))
        (srcmax (array-total-size src))
        (srcoffset 0))
    (setf (row-major-aref dst 0) (row-major-aref src 0))
    (with-plusp-indices (idx newrow? dstdims)
      (when newrow?
        (when (>= (incf srcoffset srclen) srcmax)
          (setf srcoffset 0)))
      (setf srcidx (mod (1+ srcidx) srclen))
      (setf (apply #'aref dst idx) (row-major-aref src (+ srcoffset srcidx))))
    dst))

; PORTABILITY does any implementation fiddle with the 'size' seen by the
; 'index' range during the loop? if so copy 'size' to another variable
; and decf that
(defun morna-noise! (src fill percent &aux (size (array-total-size src)))
  "Fill src with the percent amount of fill elements."
  (loop with to-fill = (truncate (* size percent))
        for index from 0 below size
        while (plusp to-fill) do
        (when (< (random 1.0) (/ to-fill size))
          (setf (row-major-aref src index)
                (if (functionp fill) (funcall fill to-fill size) fill))
          (decf to-fill))
        (decf size))
  src)

; generic rotate would need some means for what dimension(s) are being
; rotated, by how much, and in what direction. and math!!
;(defun morna-rotate (src &rest unknown) (error "TODO") src)

; special case for 2D array as those are pretty common. you can have any
; rotation direction provided it is the one that makes sense to me
(defun morna-rotate-grid (src degrees)
  "Rotate src by one of :90 :180 :270 degrees."
  (declare
    (inline morna-clone morna-flip-both! rotate-90 rotate-270)
    (type (array * (* *)) src))
  (ecase degrees
    (:90  (rotate-90 src))
    (:180 (morna-flip-both! (morna-clone src)))
    (:270 (rotate-270 src))))

; view-x, view-y being the upper left corner whence the copy starts.
; this may or may not be inside of src. NOTE that dst is modified, but
; not src. something like this could be used for field of view (FOV) of
; a world map into a smaller display area
(defun morna-subrect (src dst view-x view-y view-width view-height
                      &optional (filler #\?))
  "Copy a rectangle out of src into dst otherwise writing filler."
  (declare (type (array * (* *)) src dst))
  (loop for src-y from view-y
        for dst-y from 0
        repeat view-height do
        (loop for src-x from view-x
              for dst-x from 0
              repeat view-width do
              (setf (aref dst dst-y dst-x)
                    (if (array-in-bounds-p src src-y src-x)
                      (aref src src-y src-x)
                      filler))))
  dst)

; sugar for a morna-crop special case
(defun morna-trim (src width &aux (len (array-rank src)))
  "Crop all sides by width."
  (let ((trim (loop repeat len collect width)))
    (morna-crop src trim trim)))

(defun morna-truncate (src limits)
  "Truncate src to the given size for each dimension."
  (let ((dst (make-array limits :element-type (array-element-type src))))
    (setf (row-major-aref dst 0) (row-major-aref src 0))
    (with-plusp-indices (idx newrow? (array-dimensions src))
      (when (apply #'array-in-bounds-p dst idx)
        (setf (apply #'aref dst idx) (apply #'aref src idx))))
    dst))

; it's probably easier to show than tell what morna-upfrac does;
;
;   #.
;   ##
;
; can upfrac to
;   
;   #...
;   ##..
;   #.#.
;   ####
;
; if #\. is fill and #\# is true via test-fn. see also upvary.lisp in
; the ministry-of-silly-vaults repository
(defun morna-upfrac (src fill test-fn &optional (sources nil so?))
  "Double src to dst tiling src but only where src is true for test-fn."
  (declare (inline upfrac-pic))
  (let* ((srcdim (array-dimensions src))
         (dst (make-array (mapcar (lambda (x) (* 2 x)) srcdim)
                          :element-type (array-element-type src)
                          :initial-element fill)))
    (when (funcall test-fn (row-major-aref src 0))
      (morna-copy! dst (upfrac-pick so? src sources)))
    (with-plusp-indices (srcidx roll? srcdim)
      (when (funcall test-fn (apply #'aref src srcidx))
        (morna-copy! dst (upfrac-pick so? src sources)
                     (mapcar (lambda (x) (* 2 x)) srcidx))))
    dst))

(defun morna-write-file (src pathname)
  "Write src to the given file."
  (declare (type (array * (* *)) src))
  (with-open-file (stream pathname :direction :output
                          :if-exists :supersede)
    (morna-display-grid src stream)))
