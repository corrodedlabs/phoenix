;; depends on shaderc library
;; defined in shaderc.scm at the root

(define map-indexed
  (lambda (f arr)
    (let lp ((i 0)
	     (xs arr)
	     (coll '()))
      (cond
       ((null? xs) (reverse coll))
       (else (lp (+ i 1)
		 (cdr xs)
		 (cons (f (car xs) i) coll)))))))

;; (map-indexed (lambda (e i) (+ e i)) '(12 34 5))



(define create-shader-stages
  (lambda (device vertex-shader-filename fragment-shader-filename)
    
    (define create-shader-module
      (lambda (spv-array-pointer)
	(let ((info (make-vk-shader-module-create-info shader-module-create-info 0 0
						       (array-pointer-length spv-array-pointer)
						       (array-pointer-raw-ptr spv-array-pointer)))
	      (module (make-foreign-object vk-shader-module)))
	  (vk-create-shader-module device info 0 module)
	  module)))

    (define create-shader-stage-info
      (lambda (shader-stage-bit module)
	(make-vk-pipeline-shader-stage-create-info pipeline-shader-stage-create-info 0 0
						   shader-stage-bit
						   (pointer-ref-value module)
						   "main"
						   0)))
    
    (let ((vertex-spv (compile-shaders vertex-shader-filename shaderc-vertex-shader))
	  (frag-spv (compile-shaders fragment-shader-filename shaderc-fragment-shader)))
      (list (create-shader-stage-info vk-shader-stage-vertex-bit
				      (create-shader-module vertex-spv))
	    (create-shader-stage-info vk-shader-stage-fragment-bit
				      (create-shader-module frag-spv))))))


;; vertex input 

(define-record-type vertex-input (fields position color))

(define vertex-input-total-length
  (lambda (input)
    (fx+ (vector-length (vertex-input-position input))
	 (vector-length (vertex-input-color input)))))

(define displayln
  (lambda (tag x) (display tag) (display ":") (display x) (newline)))

(define bytevector-vertex-input-set!
  (case-lambda
    ((bv input) (bytevector-vertex-input-set! bv input 0))
    ((bv input offset)
     (let ((vals (vector->list (vector-append (vertex-input-position input)
					      (vertex-input-color input)))))
       (map-indexed (lambda (value i)
		      (displayln "value" value)
		      (displayln "i" i)
		      (displayln "offset" offset)
		      (bytevector-ieee-single-native-set! bv (+ (* 4 i) offset) value))
		    vals)
       (* 4 (length vals))))))


(define vertices->bytevector
  (lambda (vertices)
    (fold-left (lambda (i+bv e)
		 (let ((i (car i+bv))
		       (bv (cdr i+bv)))
		   (cons (fx+ 4 (bytevector-vertex-input-set! bv e i))
			 bv)))
	       (cons 0 (make-bytevector (fx* 4 vertices-length)))
	       vertices)))

(define vector->attr
  (lambda (v)
    (case (vector-length v)
      ((2) (cons 8 vk-format-r32g32-sfloat))
      ((3) (cons 12 vk-format-r32g32b32-sfloat))
      (else (error "unsupported vector" v)))))

(define vertex-input->attrs
  (lambda (input)
    (list (vector->attr (vertex-input-position input)) 
	  (vector->attr (vertex-input-color input)))))


;; setup vertex input descriptors
;; attrs is a list of (format . offset)
;; todo may be it should be in records
(define create-vertex-input
  (lambda (stride attrs)

    (define make-vertex-binding-description
      (lambda ()
	(make-vk-vertex-input-binding-description 0 stride vk-vertex-input-rate-vertex)))


    (define make-attribute-descriptions
      (lambda ()
	(array-pointer-raw-ptr
	 (list->vk-vertex-input-attribute-description-pointer-array
	  (map-indexed (lambda (attr i)
			 (displayln "attr" attr)
			 (make-vk-vertex-input-attribute-description i
								     0
								     (car attr)
								     (cdr attr)))
		       attrs)))))
    
    (make-vk-pipeline-vertex-input-state-create-info pipeline-vertex-input-state-create-info
						     0
						     0
						     1
						     (make-vertex-binding-description)
						     (length attrs)
						     (make-attribute-descriptions))))


#!eof

(load "vulkan/pipeline.scm")
(create-shader-stages (vulkan-state-device vs)
		      "shaders/shader.vert"
		      "shaders/shader.frag")


(make-bytevector (*  4))
(bytevector-ieee-double-set!)


(define vertices (list (make-vertex-input '#2( 0.0 -0.5) '#3(1.0 0.0 0.0))
		       (make-vertex-input '#2( 0.5  0.5) '#3(0.0 1.0 0.0))
		       (make-vertex-input '#2(-0.5  0.5) '#3(0.0 0.0 1.0))))

(define vertices-length (fold-left + 0 (map vertex-input-total-length vertices)))

(vertices->bytevector vertices)

(define stride 4)

(define attrs (vertex-input->attrs (car vertices)))

(define vertex-input-create-info (create-vertex-input stride attrs))
