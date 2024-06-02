(define π 3.14159265358979323846264338327950288)
(define degree->radian (lambda (deg) (fl* deg (/ π 180))))
(define zero 0.0)

(define x-axis (make-vector3 1.0 0.0 0.0))
(define y-axis (make-vector3 0.0 1.0 0.0))
(define z-axis (make-vector3 0.0 0.0 1.0))

;; create a homegenous transformation matrix from a rotation around the `z` axis
;; (roll)
(define from-angle-z
  (lambda (theta)
    (let* ((radian (degree->radian theta))
	   (s (sin radian))
	   (c (cos radian)))
      (make-matrix4 (make-vector4     c    s  zero zero)
		    (make-vector4 (fl- s)    c  zero zero)
		    (make-vector4  zero zero   1.0 zero)
		    (make-vector4  zero zero  zero  1.0)))))


;; matrix  is matrix4
;; angle is in radian
;; axis is vector3
(define rotate-matrix
  (lambda (matrix θ axis)
    (let* ((c (cos θ))
	   (s (sin θ))
	   (norm-axis (normalize-vector axis))
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

;; translate matrix4 by vector3
(define translate-matrix
  (lambda (matrix vector)
    (match-let ((($ matrix4 r0 r1 r2 r3) matrix)
		(($ vector3 x y z) vector))
      (make-matrix4 r0
		    r1
		    r2
		    (vector4+ (scale-vector r0 x)
			      (scale-vector r1 y)
			      (scale-vector r2 z)
			      r3)))))

(define scale-matrix
  (lambda (matrix vector)
    (match-let ((($ matrix4 m0 m1 m2 m3) matrix)
		(($ vector3 x y z) vector))
      (make-matrix4 (scale-vector m0 x)
		    (scale-vector m1 y)
		    (scale-vector m2 z)
		    m3))))

;; Create a perspective projection matrix.
(define opengl-perspective-matrix
  (lambda (fovy aspect near far)
    (let* ((θ (fl* 0.5 (degree->radian fovy)))
	   (y-scale (/ 1.0 (tan θ)))
	   (x-scale (/ y-scale aspect))
	   (fn (fl/ 1.0 (fl- near far))))
      (make-matrix4 (make-vector4 x-scale zero zero zero)
		    (make-vector4 zero y-scale zero zero)
		    (make-vector4 zero
				  zero
				  (fl* far fn)
				  -1.0)
		    (make-vector4 zero
				  zero
				  (* near far fn)
				  zero)))))

(define look-at
  (lambda (position target up)
    (let* ((f (normalize-vector (vector3- target position)))
	   (s (normalize-vector (vector-cross f up)))
	   (u (vector-cross s f)))
      (make-matrix4 (make-vector4 (vector3-x s) (vector3-x u) (- (vector3-x f)) 0.0)
		    (make-vector4 (vector3-y s) (vector3-y u) (- (vector3-y f)) 0.0)
		    (make-vector4 (vector3-z s) (vector3-z u) (- (vector3-z f)) 0.0)
		    (make-vector4 (- (vector-dot s position))
				  (- (vector-dot u position))
				  (vector-dot f position)
				  1.0)))))
