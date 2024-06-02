(define-record-type matrix4
  (nongenerative)
  (fields x y z w))

(define matrix->list
  (lambda (matrix)
    (match matrix
      (($ matrix4 x y z w)
       (apply append (map vector->list (list x y z w)))))))

(define display-matrix
  (lambda (matrix)
    (displayln "matrix contents")
    (match matrix
      (($ matrix4 x y z w)
       (for-each (lambda (v)
		   (displayln (vector->list v))) (list x y z w))))))


(define transpose-matrix4
  (lambda (mat)
    (match mat
      (($ matrix4 x y z w)
       (match x
	 (($ vector4 x1 x2 x3 x4)
	  (match y
	    (($ vector4 y1 y2 y3 y4)
	     (match z
	       (($ vector4 z1 z2 z3 z4)
		(match w
		  (($ vector4 w1 w2 w3 w4)
		   (make-matrix4 (make-vector4 x1 y1 z1 w1)
				 (make-vector4 x2 y2 z2 w2)
				 (make-vector4 x3 y3 z3 w3)
				 (make-vector4 x4 y4 z4 w4))))))))))))))

(define constant-matrix4
  (lambda (constant)
    (make-matrix4 (constant-vector4 constant)
		  (constant-vector4 constant)
		  (constant-vector4 constant)
		  (constant-vector4 constant))))

(define identity-matrix4
  (lambda ()
    (make-matrix4 (make-vector4 1.0 0.0 0.0 0.0)
		  (make-vector4 0.0 1.0 0.0 0.0)
		  (make-vector4 0.0 0.0 1.0 0.0)
		  (make-vector4 0.0 0.0 0.0 1.0))))

(define test-matrix  (make-matrix4 (make-vector4 1 2 3 4)
  				   (make-vector4 1 2 3 4)
  				   (make-vector4 1 2 3 4)
  				   (make-vector4 1 2 3 4)))

(define matrix4*
  (lambda (mat1 mat2)
    (match mat1
      (($ matrix4 x1 y1 z1 w1)
       (match (transpose-matrix4 mat2)
	 (($ matrix4 x2 y2 z2 w2)
	  (make-matrix4
	   (make-vector4 (vector4* x1 x2)
			 (vector4* x1 y2)
			 (vector4* x1 z2)
			 (vector4* x1 w2))
	   (make-vector4 (vector4* y1 x2)
			 (vector4* y1 y2)
			 (vector4* y1 z2)
			 (vector4* y1 w2))
	   (make-vector4 (vector4* z1 x2)
			 (vector4* z1 y2)
			 (vector4* z1 z2)
			 (vector4* z1 w2))
	   (make-vector4 (vector4* w1 x2)
			 (vector4* w1 y2)
			 (vector4* w1 z2)
			 (vector4* w1 w2)))))))))

