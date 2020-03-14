(library (camera)
  ;; exporting function necessary to provide a view into the world created
  ;; this data will be used by the uniform buffer
  (export calculate-mvp-matrix
	  mvp-matrix->list)

  (import (except (scheme) vector->list)
	  (matchable))

  (define-record-type point3 (fields x y z))
  (define-record-type vector3 (fields x y z))

  (define-record-type vector4 (fields x y z w))
  (define-record-type matrix4 (fields x y z w))

  (define vector->list
    (lambda (vector)
      (match vector
	(($ vector4 x y z w) (list x y z w)))))

  (define matrix->list
    (lambda (matrix)
      (match matrix
	(($ matrix4 x y z w) (apply append (map vector->list (list x y z w)))))))

  (define π 3.14159265358979323846264338327950288)

  (define degree->radian (lambda (deg) (* deg (/ π 180))))

  (define zero 0.0)


  ;; create a homegenous transformation matrix from a rotation around the `z` axis (roll).
  (define from-angle-z
    (lambda (theta)
      (let* ((radian (degree->radian theta))
	     (s (sin radian))
	     (c (cos radian)))
	(make-matrix4 (make-vector4     c    s  zero zero)
		      (make-vector4 (- s)    c  zero zero)
		      (make-vector4  zero zero   1.0 zero)
		      (make-vector4  zero zero  zero  1.0)))))

  (define vector-
    (lambda (p1 p2)
      (make-vector3 (- (vector3-x p1) (vector3-x p2))
		    (- (vector3-y p1) (vector3-y p2))
		    (- (vector3-z p1) (vector3-z p2)))))

  (define vector-mod
    (lambda (value)
      (match value
	(($ vector3 x y z) (sqrt (+ (* x x) (* y y) (* z z)))))))


  ;; normalize the given vector
  (define normalize
    (lambda (vector)
      (match vector
	(($ vector3 x y z)
	 (let ((mod-value (vector-mod vector)))
	   (make-vector3 (/ x mod-value) (/ y mod-value) (/ z mod-value)))))))

  ;; cross product b/w two vectors
  (define cross
    (lambda (vec1 vec2)
      (match vec1
	(($ vector3 x1 y1 z1)
	 (match vec2
	   (($ vector3 x2 y2 z2) (make-vector3 (- (* z2 y1) (* z1 x2))
					       (- (* z1 x2) (* x1 z2))
					       (- (* x1 y2) (* y1 x2)))))))))

  (define dot
    (lambda (vec1 vec2)
      (+ (* (vector3-x vec1) (vector3-x vec2))
	 (* (vector3-y vec1) (vector3-y vec2))
	 (* (vector3-z vec1) (vector3-z vec2)))))

  (define look-at
    (lambda (eye center up)
      (let* ((look-at-direction (vector- center eye))
	     (f (normalize look-at-direction))
	     (s (normalize (cross f up)))
	     (u (cross s f)))
	(make-matrix4 (make-vector4   (vector3-x s)   (vector3-x u)   (- (vector3-x f)) zero)
		      (make-vector4   (vector3-y s)   (vector3-y u)   (- (vector3-y f)) zero)
		      (make-vector4   (vector3-z s)   (vector3-z u)   (- (vector3-z f)) zero)
		      (make-vector4   (- (dot eye s)) (- (dot eye u)) (dot eye f)       1.0)))))



  ;; Create a perspective projection matrix.
  (define perspective
    (lambda (fovy aspect near far)
      (let* ((θ (* 0.5 (degree->radian fovy)))
	     (y-scale (/ 1.0 (tan θ)))
	     (x-scale (/ y-scale aspect)))
	(make-matrix4 (make-vector4 x-scale zero    zero zero)
		      (make-vector4 zero    (- y-scale) zero zero)
		      (make-vector4 zero    zero    (/ (- (+ far near)) (- far near)) -1.0)
		      (make-vector4 zero    zero    (/ (- (* 2 near far)) (- far near)) zero)))))


  (define-record-type mvp-matrix (fields model view projection))

  (define mvp-matrix->list
    (lambda (mvp-matrix-obj)
      (match mvp-matrix-obj
	(($ mvp-matrix model view projection)
	 (apply append (map matrix->list (list model view projection)))))))

  (define calculate-mvp-matrix
    (lambda (screen-width screen-height)
      (let ((eye (make-vector3 2.0 2.0 2.0))
	    (center (make-vector3 0.0 0.0 0.0))
	    (up (make-vector3 0.0 0.0 1.0))
	    (fovy 45)
	    (aspect (/ screen-width screen-height))
	    (near 0.1)
	    (far 10.0))
	(make-mvp-matrix (from-angle-z 90)
			 (look-at eye center up)
			 (perspective fovy aspect near far))))))


#|

Usage and tests:

(define theta 90)
(degree->radian 90)
(inexact->exact  (vector4-x (matrix4-x (from-angle-z 90))))

(define eye (make-vector3 2.0 2.0 2.0))
(define center (make-vector3 0.0 0.0 0.0))
(define up (make-vector3 0.0 0.0 1.0))

(define dir (vector- center eye))



(define model (from-angle-z 90))
(define view (look-at eye center up))

(define fovy 45)
(define aspect (/ 800 600))
(define near 0.1)
(define far 10.0)

(perspective fovy aspect near far)

(calculate-mvp-matrix 800 600)

|#
