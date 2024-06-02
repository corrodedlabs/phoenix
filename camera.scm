(library (camera)
  ;; exporting function necessary to provide a view into the world created
  ;; this data will be used by the uniform buffer
  (export calculate-mvp-matrix
	  update-mvp-matrix
	  mvp-matrix->list
	  make-vector3
	  camera
	  default-camera
	  update-camera
	  update-camera-front
	  forward
	  back
	  right
	  left)

  (import (except (scheme) vector->list)
	  (matchable)
	  (prelude)
	  (math))

  ;; (vec3 vec3) => mat4
  (define update-view-matrix
    (lambda (position rotation)
      (match-let ((($ vector3 rotx roty rotz) rotation))
	(let ((rotation-matrix
	       (rotate-matrix (rotate-matrix (rotate-matrix (identity-matrix4 1.0)
							    (degree->radian rotx)
							    x-axis)
					     (degree->radian roty)
					     y-axis)
			      (degree->radian rotz)
			      z-axis))
	      (translation-matrix (translate-matrix (identity-matrix4 1.0) position)))
	  (matrix4* translation-matrix rotation-matrix)))))

  (define-record-type mvp-matrix
    (nongenerative)
    (fields model view projection eye look-at-direction))

  (define mvp-matrix->list
    (lambda (mvp-matrix-obj)
      (match mvp-matrix-obj
	(($ mvp-matrix model view projection)
	 (apply append (map matrix->list (list model view projection)))))))

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
	(make-mvp-matrix  (rotate-matrix (identity-matrix4 1.0)
					 (degree->radian (- 90.0))
					 y-axis)
			  (update-view-matrix eye camera-rotation)
			  (opengl-perspective-matrix fovy aspect near far)
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
	      (let ((camera-front
		     (normalize-vector (make-vector3
					(fl* (fl- (cos (degree->radian rotx)))
					     (sin (degree->radian roty)))

					(sin (degree->radian rotx))

					(fl* (cos (degree->radian rotx))
					     (cos (degree->radian roty)))))))
		;; (displayln "time diff" (time-difference t1 t0))
		(set! t0 t1)
		(case direction
		  ((forward)
		   (vector3+ eye (scale-vector camera-front δ)))
		  
		  ((back)
		   (vector3- eye (scale-vector camera-front δ)))
		  
		  ((right)
		   (vector3+ eye
			     (scale-vector (normalize-vector
					    (vector-cross camera-front y-axis))
					   δ)))
		  ((left)
		   (vector3- eye
			     (scale-vector (normalize-vector
					    (vector-cross camera-front y-axis))
					   δ)))
		  
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
	 (let ((eye (car (update-eye-center eye-position center))))
	   (let ((view (update-view-matrix eye camera-rotation)))
	     (cons (make-mvp-matrix model
				    view
				    projection
				    eye
				    center) eye)))))))

  (define-record-type rotation-params
    (nongenerative camera-rotation-params)
    (fields last-x last-y yaw pitch))

  (define-record-type camera
    (nongenerative camera)
    (fields position front up rotation-data))

  (define default-camera
    (lambda ()
      (make-camera (make-vector3 0.0 0.0  3.0)
		   (make-vector3 0.0 0.0 -1.0)
		   (make-vector3 0.0 1.0  0.0)
		   (make-rotation-params #f #f -90.0 0.0))))

  (define update-camera-position
    (lambda (camera position)
      (make-camera position
		   (camera-front camera)
		   (camera-up camera)
		   (camera-rotation-data camera))))

  (define update-camera-front
    (lambda (camera front)
      (make-camera (camera-position camera)
		   front
		   (camera-up camera)
		   (camera-rotation-data camera))))

  (define update-camera-rotation-data
    (lambda (camera rotation-data)
      (make-camera (camera-position camera)
		   (camera-front camera)
		   (camera-up camera)
		   rotation-data)))

  (define *camera-speed* 0.05)
  (define *mouse-sensitivity* 0.1)

  (define update-camera
    (lambda (cam movement-direction delta mouse-coords)

      (define calculate-camera-front
	(lambda (yaw pitch)
	  ;; (displayln "yaw" yaw)
	  ;; (displayln "pitch" pitch)
	  (let ((yaw-radian (degree->radian yaw))
		(pitch-radian (degree->radian pitch)))
	    (normalize-vector (make-vector3 (* (cos yaw-radian)
					       (cos pitch-radian))
					    (sin pitch-radian)
					    (* (sin yaw-radian)
					       (cos pitch-radian)))))))

      (define calculate-mouse-rotation
	(lambda (cam)
	  ;; if there is no last-x and last-y recorded then use the one
	  ;; from mouse-coords for a smooth experience when the first
	  ;; callback is triggerred
	  (match-let ((($ rotation-params last-x last-y yaw pitch)
		       (camera-rotation-data cam)))
	    (let* ((cur-x (car mouse-coords))
		   (cur-y (cdr mouse-coords))
		   (last-x (or last-x cur-x))
		   (last-y (or last-y cur-y))
		   (yaw (+ yaw (* (- cur-x last-x) *mouse-sensitivity*)))
		   (pitch 
		    (clamp-between -89.0
				   (+ pitch (* (- cur-y last-y) *mouse-sensitivity*))
				   89.0))
		   (rotation-data
		    (make-rotation-params cur-x
					  cur-y
					  yaw
					  pitch)))
	      (update-camera-front (update-camera-rotation-data cam rotation-data)
				   (calculate-camera-front yaw pitch))))))

      (define calculate-mouse-translation
	(lambda (cam)
	  (match-let ((($ camera position front up) cam))
	    (let* ((camera-speed (* 2.5 delta))
		   (new-position
		    (case movement-direction
		      (forward
		       (vector3+ position (scale-vector front camera-speed)))
		      (back
		       (vector3- position (scale-vector front camera-speed)))
		      (left
		       (vector3- position
				 (scale-vector
				  (normalize-vector (vector-cross front up))
				  camera-speed)))
		      (right
		       (vector3+ position
				 (scale-vector
				  (normalize-vector (vector-cross front up))
				  camera-speed)))
		      (else position))))
	      (update-camera-position cam new-position)))))
      
      (calculate-mouse-translation (calculate-mouse-rotation cam)))))


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
