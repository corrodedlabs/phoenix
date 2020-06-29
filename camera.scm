(library (camera)
  ;; exporting function necessary to provide a view into the world created
  ;; this data will be used by the uniform buffer
  (export calculate-mvp-matrix
	  update-mvp-matrix
	  mvp-matrix->list
	  make-vector3
	  normalize)

  (import (except (scheme) vector->list)
	  (matchable)
	  (prelude))

  ;; (define o (load-shared-object "./phoenix-libs/glm_bridge.so"))

  ;; (define-ftype vec3 uptr)
  ;; (define-ftype mat4 uptr)

  ;; (define make-vec3 (foreign-procedure "vec3" (float float float) vec3))
  ;; (define look_at (foreign-procedure "look_at" (vec3 vec3 vec3) mat4))

  (define-record-type point3 (fields x y z))
  (define-record-type vector3 (fields x y z))

  (define-record-type vector4 (fields x y z w))
  (define-record-type matrix4 (fields x y z w))

  (define constant-vector4
    (lambda (constant)
      (make-vector4 constant constant constant constant)))

  (define vector->list
    (lambda (vector)
      (match vector
	(($ vector4 x y z w) (list x y z w))
	(($ vector3 x y z)   (list x y z)))))

  (define matrix->list
    (lambda (matrix)
      (match matrix
	(($ matrix4 x y z w) (apply append (map vector->list (list x y z w)))))))

  (define display-matrix
    (lambda (matrix)
      (displayln "matrix contents")
      (match matrix
	(($ matrix4 x y z w)  (for-each (lambda (v)
					  (displayln (vector->list v))) (list x y z w))))))

  (define π 3.14159265358979323846264338327950288)

  (define degree->radian (lambda (deg) (fl* deg (/ π 180))))

  (define zero 0.0)


  ;; create a homegenous transformation matrix from a rotation around the `z` axis (roll).
  (define from-angle-z
    (lambda (theta)
      (let* ((radian (degree->radian theta))
	     (s (sin radian))
	     (c (cos radian)))
	(make-matrix4 (make-vector4     c    s  zero zero)
		      (make-vector4 (fl- s)    c  zero zero)
		      (make-vector4  zero zero   1.0 zero)
		      (make-vector4  zero zero  zero  1.0)))))

  (define vector3+
    (lambda (vec1 vec2)
      (match vec1
	(($ vector3 x1 y1 z1)
	 (match vec2
	   (($ vector3 x2 y2 z2)
	    (make-vector3 (fl+ x1 x2) (fl+ y1 y2) (fl+ z1 z2))))))))

  (define vector3-
    (lambda (p1 p2)
      (make-vector3 (fl- (vector3-x p1) (vector3-x p2))
		    (fl- (vector3-y p1) (vector3-y p2))
		    (fl- (vector3-z p1) (vector3-z p2)))))

  (define (vector4+ . vectors)
    (fold-left (lambda (res vec)
		 (match res
		   (($ vector4 x1 y1 z1 w1)
		    (match vec
		      (($ vector4 x2 y2 z2 w2)
		       (make-vector4 (fl+ x1 x2) (fl+ y1 y2) (fl+ z1 z2) (fl+ w1 w2))))))		 )
	       (make-vector4 0.0 0.0 0.0 0.0)
	       vectors))

  ;; (vector4+ (make-vector4 1 2 3 4) (make-vector4 5 4 3 2) (make-vector4 3 2 1 1))

  (define vector4-
    (lambda (p1 p2)
      (make-vector4 (fl- (vector4-x p1) (vector4-x p2))
		    (fl- (vector4-y p1) (vector4-y p2))
		    (fl- (vector4-z p1) (vector4-z p2))
		    (fl- (vector4-w p1) (vector4-w p2)))))

  (define vector4*
    (lambda (vec1 vec2)
      (fl+ (fl* (vector4-x vec1) (vector4-x vec2))
	   (fl* (vector4-y vec1) (vector4-y vec2))
	   (fl* (vector4-z vec1) (vector4-z vec2))
	   (fl* (vector4-w vec1) (vector4-w vec2)))))
  
  (define scale-vector
    (lambda (vector scale)
      (match vector
	(($ vector3 x y z)
	 (make-vector3 (fl* scale x) (fl* scale y) (fl* scale z)))
	(($ vector4 x y z w)
	 (make-vector4 (fl* scale x) (fl* scale y) (fl* scale z) (fl* scale w))))))

  (define vector-mod
    (lambda (value)
      (match value
	(($ vector3 x y z) (sqrt (fl+ (fl* x x) (fl* y y) (fl* z z))))
	(($ vector4 x y z w) (sqrt (fl+ (fl* x x) (fl* y y) (fl* z z) (fl* w w)))))))

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

  (define diagonal-matrix4
    (lambda (value)
      (make-matrix4 (make-vector4 1.0 0.0 0.0 0.0)
		    (make-vector4 0.0 1.0 0.0 0.0)
		    (make-vector4 0.0 0.0 1.0 0.0)
		    (make-vector4 0.0 0.0 0.0 1.0))))

  (define test-matrix  (make-matrix4 (make-vector4 1.0 2.0 3.0 4.0)
  				     (make-vector4 1.0 2.0 3.0 4.0)
  				     (make-vector4 1.0 2.0 3.0 4.0)
  				     (make-vector4 1.0 2.0 3.0 4.0)))

  (define matrix4-identity (make-matrix4 (make-vector4 1 0 0 0)
  					 (make-vector4 0 1 0 0)
  					 (make-vector4 0 0 1 0)
  					 (make-vector4 0 0 0 1)))

  ;; (display-matrix test-matrix)
  
  ;; (define m2 (transpose-matrix4 test-matrix))

  (define matrix4*
    (lambda (mat1 mat2)
      (match mat1
	(($ matrix4 x1 y1 z1 w1)
	 (match (transpose-matrix4 mat2)
	   (($ matrix4 x2 y2 z2 w2)
	    (make-matrix4
	     (make-vector4 (vector4* x1 x2) (vector4* x1 y2) (vector4* x1 z2) (vector4* x1 w2))
	     (make-vector4 (vector4* y1 x2) (vector4* y1 y2) (vector4* y1 z2) (vector4* y1 w2))
	     (make-vector4 (vector4* z1 x2) (vector4* z1 y2) (vector4* z1 z2) (vector4* z1 w2))
	     (make-vector4 (vector4* w1 x2) (vector4* w1 y2) (vector4* w1 z2) (vector4* w1 w2)))))))))

  ;; (display-matrix (matrix4* test-matrix matrix4-identity))

  ;; normalize the given vector
  (define normalize
    (lambda (vector)
      (match vector
	(($ vector3 x y z)
	 (let ((mod-value (vector-mod vector)))
	   (make-vector3 (/ x mod-value) (/ y mod-value) (/ z mod-value))))
	(($ vector4 x y z w)
	 (let ((mod-value (vector-mod vector)))
	   (make-vector4 (/ x mod-value) (/ y mod-value) (/ z mod-value) (/ w mod-value)))))))

  ;; cross product b/w two vectors
  (define cross
    (lambda (vec1 vec2)
      (match vec1
	(($ vector3 x1 y1 z1)
	 (match vec2
	   (($ vector3 x2 y2 z2) (make-vector3 (fl- (fl* z2 y1) (fl* z1 x2))
					       (fl- (fl* z1 x2) (fl* x1 z2))
					       (fl- (fl* x1 y2) (fl* y1 x2)))))))))

  (define dot
    (lambda (vec1 vec2)
      (fl+ (fl* (vector3-x vec1) (vector3-x vec2))
	   (fl* (vector3-y vec1) (vector3-y vec2))
	   (fl* (vector3-z vec1) (vector3-z vec2)))))



  ;; matrix  is matrix4
  ;; angle is in radian
  ;; axis is vector3
  (define rotate
    (lambda (matrix θ axis)
      (let* ((c (cos θ))
	     (s (sin θ))
	     (norm-axis (normalize axis))
	     (temp (scale-vector axis (fl- 1.0 c))))
	(match norm-axis
	  (($ vector3 ax ay az)
	   (match temp
	     (($ vector3 tx ty tz)
	      ;; rotation matrix elements
	      (let ((r00 (fl+ c (fl* tx ax)))
		    (r01 (fl+ (fl* s az) (fl* tx ay)))
		    (r02 (fl- (fl* tx az) (fl* s ay)))

		    (r10 (fl- (fl* ty ax) (fl* s az)))
		    (r11 (fl+ c (fl* ty ay)))
		    (r12 (fl+ (fl* ty az) (fl* s ax)))

		    (r20 (fl+ (fl* tz ax) (fl* s ay)))
		    (r21 (fl- (fl* tz ay) (fl* s ax)))
		    (r22 (fl+ c (fl* tz az))))
		(match matrix
		  (($ matrix4 m0 m1 m2 m3)
		   (make-matrix4 (vector4+ (scale-vector m0 r00)
					   (scale-vector m1 r01)
					   (scale-vector m2 r02))
				 (vector4+ (scale-vector m0 r10)
					   (scale-vector m1 r11)
					   (scale-vector m2 r12))
				 (vector4+ (scale-vector m0 r20)
					   (scale-vector m1 r21)
					   (scale-vector m2 r22))
				 m3)))))))))))

  ;; (display-matrix (rotate test-matrix (degree->radian 90) (make-vector3 0 0 1)))
  
  ;; translate matrix4 by vector3
  (define translate
    (lambda (matrix vector)
      (match-let ((($ matrix4 r0 r1 r2 r3) matrix)
		  (($ vector3 x y z) vector))
	(make-matrix4 r0
		      r1
		      r2
		      (vector4+ (scale-vector r0 x) (scale-vector r1 y) (scale-vector r2 z) r3)))))

  ;; (display-matrix (translate test-matrix (make-vector3 1 2 3)))
  (define x-axis (make-vector3 1.0 0.0 0.0))
  (define y-axis (make-vector3 0.0 1.0 0.0))
  (define z-axis (make-vector3 0.0 0.0 1.0))

  ;; (vec3 vec3) => mat4
  (define update-view-matrix
    (lambda (position rotation)
      (match-let ((($ vector3 rotx roty rotz) rotation))
	(let ((rotation-matrix
	       (rotate (rotate (rotate (diagonal-matrix4 1.0) (degree->radian rotx) x-axis)
			       (degree->radian roty)
			       y-axis)
		       (degree->radian rotz)
		       z-axis))
	      (translation-matrix (translate (diagonal-matrix4 1.0) position)))
	  (matrix4* translation-matrix rotation-matrix)))))



  ;; Create a perspective projection matrix.
  (define perspective
    (lambda (fovy aspect near far)
      (let* ((θ (fl* 0.5 (degree->radian fovy)))
	     (y-scale (/ 1.0 (tan θ)))
	     (x-scale (/ y-scale aspect)))
	(make-matrix4 (make-vector4 x-scale zero    zero zero)
		      (make-vector4 zero    (fl- y-scale) zero zero)
		      (make-vector4 zero    zero    (/ (fl- (fl+ far near)) (fl- far near)) -1.0)
		      (make-vector4 zero    zero    (/ (fl- (fl* 2.0 near far)) (fl- far near)) zero)))))


  (define-record-type mvp-matrix (fields model view projection eye look-at-direction))

  (define mvp-matrix->list
    (lambda (mvp-matrix-obj)
      (match mvp-matrix-obj
	(($ mvp-matrix model view projection eye)
	 (append (apply append (map matrix->list (list model view projection)))
		 (vector->list eye))))))

  (define up (make-vector3 0.0 1.0 0.0))

  (define eye-position (make-vector3 10.0 13.0 1.8))
  (define camera-rotation (make-vector3 (- 62.5) 90.0 0.0))
  
  (define calculate-mvp-matrix
    (lambda (screen-width screen-height)
      (let ((eye eye-position)
	    (center (make-vector3 0.0 0.0 0.0))
	    (fovy 60.0)
	    (aspect (fx/ screen-width screen-height))
	    (near 0.1)
	    (far 1000.0))
	(make-mvp-matrix  (rotate (diagonal-matrix4 1.0)
				  (degree->radian (- 90.0))
				  y-axis)
			  (update-view-matrix eye camera-rotation)
			  (perspective fovy aspect near far)
			  eye
			  center))))

  (define-syntax forward (identifier-syntax 'forward))
  (define-syntax back (identifier-syntax 'back))
  (define-syntax right (identifier-syntax 'right))
  (define-syntax left (identifier-syntax 'left))

  (define *speed* 30.0)
  (define t0 (current-time))

  (define update-mvp-matrix
    (lambda (current-matrix eye-position movement-direction)

      (define move
	(lambda (direction eye)
	  (let* ((t1 (current-time))
		 (δ (* (/ (time-nanosecond (time-difference t1 t0))
			  1000000000)
		       *speed*)))
	    (match-let ((($ vector3 rotx roty rotz) camera-rotation))
	      ;; (displayln "camera front cal" δ)
	      (let ((camera-front (normalize (make-vector3 (fl* (fl- (cos (degree->radian rotx)))
								(sin (degree->radian roty)))

							   (sin (degree->radian rotx))

							   (fl* (cos (degree->radian rotx))
								(cos (degree->radian roty)))))))
		;; (displayln "time diff" (time-difference t1 t0))
		(set! t0 t1)
		(case direction
		  ((forward) (vector3+ eye (scale-vector camera-front δ)))
		  ((back) (vector3- eye (scale-vector camera-front δ)))
		  ((right) (vector3+ eye
				     (scale-vector (normalize (cross camera-front y-axis)) δ)))
		  ((left) (vector3- eye
				    (scale-vector (normalize (cross camera-front y-axis)) δ)))
		  (else (error "invalid direction" direction))))))))

      ;; camera update function
      (define update-eye-center
	(lambda (eye center)
	  (match movement-direction
	    (($ movement-data forward? back? right? left?)
	     (cons (cond
		    (forward? (move forward eye))
		    (back? (move back eye))
		    (right? (move right eye))
		    (left? (move left eye))
		    (else eye)) center))
	    (else (error "invalid movement direction" movement-direction)))))

      (match current-matrix
	(($ mvp-matrix model view projection eye center)
	 (let* ((eye (car (update-eye-center eye-position center)))
		(view (update-view-matrix eye camera-rotation)))
	   (cons (make-mvp-matrix model view projection eye center)
		 eye)))))))


#|

Usage and tests:

(define theta 90)
(degree->radian 90)
(inexact->exact  (vector4-x (matrix4-x (from-angle-z 90))))

(define eye (make-vector3 2.0 2.0 2.0))
(define center (make-vector3 0.0 0.0 0.0))
(define up (make-vector3 0.0 0.0 1.0))

(define dir (vector3- center eye))



(define model (from-angle-z 90))
(define view (look-at eye center up))

(define fovy 45)
(define aspect (/ 800 600))
(define near 0.1)
(define far 10.0)

(perspective fovy aspect near far)

(calculate-mvp-matrix 800 600)

|#
