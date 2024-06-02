(define-record-type point3
  (nongenerative)
  (fields x y z))

(define-record-type vector3
  (nongenerative vec3)
  (fields x y z))

(define-record-type vector4
  (nongenerative)
  (fields x y z w))

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
		     (make-vector4 (fl+ x1 x2)
				   (fl+ y1 y2)
				   (fl+ z1 z2)
				   (fl+ w1 w2)))))))
	     (make-vector4 0.0 0.0 0.0 0.0)
	     vectors))

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

(define normalize-vector
  (lambda (vector)
    (let ((mod-value (vector-mod vector)))
      (if (= 0 mod-value)
	  vector
	  (match vector
	    (($ vector3 x y z)
	     (make-vector3 (/ x mod-value) (/ y mod-value) (/ z mod-value)))
	    (($ vector4 x y z w)
	     (make-vector4 (/ x mod-value)
			   (/ y mod-value)
			   (/ z mod-value)
			   (/ w mod-value))))))))

;; cross product b/w two vectors
(define vector-cross
  (lambda (vec1 vec2)
    (match-let ((($ vector3 x1 y1 z1) vec1)
		(($ vector3 x2 y2 z2) vec2))
      (make-vector3 (fl- (fl* z2 y1) (fl* z1 y2))
		    (fl- (fl* z1 x2) (fl* x1 z2))
		    (fl- (fl* x1 y2) (fl* y1 x2))))))

;; (equal? (vector-cross (make-vector3 2.0 -4.0 4.0)
;; 		      (make-vector3 4.0 0.0 3.0))
;; 	(make-vector3 -12.0 10.0 16.0))

(define vector-dot
  (lambda (vec1 vec2)
    (fl+ (fl* (vector3-x vec1) (vector3-x vec2))
	 (fl* (vector3-y vec1) (vector3-y vec2))
	 (fl* (vector3-z vec1) (vector3-z vec2)))))

(define vector->list
  (lambda (vector)
    (match vector
      (($ vector3 x y z) (list x y z))
      (($ vector4 x y z w) (list x y z w)))))

(define constant-vector4
  (lambda (constant)
    (make-vector4 constant constant constant constant)))

(define constant-vector3
  (lambda (constant)
    (make-vector3 constant constant constant )))
