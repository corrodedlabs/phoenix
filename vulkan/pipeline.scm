;; depends on shaderc library
;; defined in shaderc.scm at the root


(define create-shader-stages
  (lambda (device vertex-shader-filename fragment-shader-filename)
    
    (define create-shader-module
      (lambda (spv-array-pointer)
	(let ((info (make-vk-shader-module-create-info shader-module-create-info 0 0
						       (array-pointer-length
							spv-array-pointer)
						       (array-pointer-raw-ptr
							spv-array-pointer)))
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

;; assuming that we are using float for everything
(define vertex-input-total-size
  (lambda (input) (fx* 4 (vertex-input-total-length input))))

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


(define (sizeof-vertex-input-arr arr)
  (fold-left + 0 (map vertex-input-total-size arr)))

(define vertices->vector
  (lambda (vertices)
    (apply vector-append
	   (map (lambda (v)
		  (vector-append (vertex-input-position v) (vertex-input-color v)))
		vertices))))

(define vertices->list
  (lambda (vertices) (vector->list (vertices->vector vertices))))


(define vertices->bytevector
  (lambda (vertices)
    (let ((vertices-length (fold-left + 0 (map vertex-input-total-length vertices))))
      (fold-left (lambda (i+bv e)
		   (let ((i (car i+bv))
			 (bv (cdr i+bv)))
		     (cons (fx+ 4 (bytevector-vertex-input-set! bv e i))
			   bv)))
		 (cons 0 (make-bytevector (fx* 4 vertices-length)))
		 vertices))))

(define vector->attr
  (lambda (v)
    (case (vector-length v)
      ((2) (cons vk-format-r32g32-sfloat 8))
      ((3) (cons vk-format-r32g32b32-sfloat 12))
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


;; input assembly

(define create-pipeline-assembly-state
  (lambda ()
    (make-vk-pipeline-input-assembly-state-create-info
     pipeline-input-assembly-state-create-info 0 0
     vk-primitive-topology-triangle-list
     vk-false)))

;; viewport

(define (create-viewport-info swapchain-extent)

  (define (create-viewport)
    (make-vk-viewport 0.0
		      0.0
		      (exact->inexact (vk-extent-2d-width swapchain-extent))
		      (exact->inexact (vk-extent-2d-height swapchain-extent))
		      0.0
		      1.0))

  (define (create-scissor)
    (make-vk-rect-2d (make-vk-offset-2d 0 0) swapchain-extent))

  (make-vk-pipeline-viewport-state-create-info pipeline-viewport-state-create-info 0 0
					       1
					       (create-viewport)
					       1
					       (create-scissor)))

;; rasterizer

(define (create-rasterizer-info)
  (make-vk-pipeline-rasterization-state-create-info pipeline-rasterization-state-create-info
						    0
						    0
						    vk-false
						    vk-false
						    vk-polygon-mode-fill
						    vk-cull-mode-back-bit
						    vk-front-face-counter-clockwise
						    vk-false
						    0.0
						    0.0
						    0.0
						    1.0))

;; multisampling

(define (create-multisampling-info)
  (make-vk-pipeline-multisample-state-create-info pipeline-multisample-state-create-info 0 0
						  vk-sample-count-1-bit
						  vk-false
						  0.0
						  (make-ftype-pointer unsigned-32 0)
						  0
						  0))

;; color blending

(define (create-color-blending-info)
  (let* ((color-write-mask (bitwise-ior vk-color-component-r-bit
					vk-color-component-g-bit
					vk-color-component-b-bit
					vk-color-component-a-bit))
	 (color-attachment (make-vk-pipeline-color-blend-attachment-state vk-false
									  0
									  0
									  0
									  0
									  0
									  0
									  color-write-mask)))
    (make-vk-pipeline-color-blend-state-create-info pipeline-color-blend-state-create-info 0 0
						    vk-false
						    vk-logic-op-copy
						    1
						    color-attachment
						    '(0.0 0.0 0.0 0.0))))


;; descriptor set layout

(define create-descriptor-layout
  (lambda (device)
    (let* ((binding
	    (make-vk-descriptor-set-layout-binding 0
						   vk-descriptor-type-uniform-buffer
						   1
						   vk-shader-stage-vertex-bit
						   (null-pointer vk-sampler)))
	   (info (make-vk-descriptor-set-layout-create-info
		  descriptor-set-layout-create-info
		  0
		  0
		  1
		  binding))
	   (layout (make-foreign-object vk-descriptor-set-layout)))
      (vk-create-descriptor-set-layout device info 0 layout)
      layout)))


;; pipeline layout

;; returns (pipeline-layout . descriptor-layout)
(define (create-pipeline-layout device)
  (let* ((descriptor-layout (create-descriptor-layout device))
	 (layout-info
	  (make-vk-pipeline-layout-create-info pipeline-layout-create-info 0 0
					       1
					       descriptor-layout
					       0
					       (null-pointer vk-push-constant-range)))
	 (layout (make-foreign-object vk-pipeline-layout)))
    (vk-create-pipeline-layout device layout-info 0 layout)
    (cons layout descriptor-layout)))

;; let's capture all the custom information that can be supplied in the pipeline in a record

;; currently supported shaders
(define-record-type shaders (fields vertex fragment))
(define-record-type vertex-input-details (fields vertex-input-list stride attrs))
(define-record-type pipeline-data (fields shaders vertex-input-details))

;; render pass

(define create-render-pass
  (lambda (device swapchain)

    ;; defined as (~0U)
    (define vk-subpass-external 0)
    
    (define create-render-pass-info
      (lambda ()
	(let* ((swapchain-image-format
		(vk-surface-format-khr-format (swapchain-format swapchain)))
	       
	       (color-attachment
		(make-vk-attachment-description 0
						swapchain-image-format
						vk-sample-count-1-bit
						vk-attachment-load-op-clear
						vk-attachment-store-op-store
						vk-attachment-load-dont-care
						vk-attachment-store-dont-care
						vk-image-layout-undefined
						vk-image-layout-present-src-khr))
	       
	       (color-attachment-ref
		(make-vk-attachment-reference 0
					      vk-image-layout-color-attachment-optimal))
	       
	       (subpass
		(make-vk-subpass-description 0
					     vk-pipeline-bind-point-graphics
					     0
					     (null-pointer vk-attachment-reference)
					     1
					     color-attachment-ref
					     (null-pointer vk-attachment-reference)
					     (null-pointer vk-attachment-reference)
					     0
					     (null-pointer unsigned-32)))
	       
	       (dependency
		(make-vk-subpass-dependency vk-subpass-external
					    0
					    vk-pipeline-stage-color-attachment-output-bit
					    vk-pipeline-stage-color-attachment-output-bit
					    0
					    (bitwise-ior
					     vk-access-color-attachment-read-bit
					     vk-access-color-attachment-write-bit)
					    0)))
	  
	  (make-vk-render-pass-create-info render-pass-create-info 0 0
					   1
					   color-attachment
					   1
					   subpass
					   1
					   dependency))))

    (let ((info (create-render-pass-info))
	  (render-pass (make-foreign-object vk-render-pass)))
      (vk-create-render-pass device info 0 render-pass)
      render-pass)))

;; record to save the pipeline information
(define-record-type pipeline
  (fields handle layout render-pass descriptor-set-layout))


(define create-graphics-pipeline
  (lambda (device swapchain pipeline-data)
    (let* ((shaders (pipeline-data-shaders pipeline-data))

	   (shader-stages (list->vk-pipeline-shader-stage-create-info-pointer-array
			   (create-shader-stages device
						 (shaders-vertex shaders)
						 (shaders-fragment shaders))))

	   (vertex-input-data (pipeline-data-vertex-input-details pipeline-data))

	   (render-pass (create-render-pass device swapchain))

	   (layout (create-pipeline-layout device))

	   (pipeline-info
	    (make-vk-graphics-pipeline-create-info
	     graphics-pipeline-create-info 0 0
	     
	     ;; shader stages
	     (array-pointer-length shader-stages)
	     (vk-pipeline-shader-stage-create-info-pointer-car shader-stages)

	     ;; vertex input 
	     (create-vertex-input (vertex-input-details-stride vertex-input-data)
				  (vertex-input-details-attrs vertex-input-data))
	     
	     (create-pipeline-assembly-state)

	     ;; tesselation
	     (null-pointer
	      vk-pipeline-tessellation-state-create-info)

	     ;; viewport
	     (create-viewport-info
	      (swapchain-extent swapchain))

	     ;; rasterizer
	     (create-rasterizer-info)

	     ;; multisampling
	     (create-multisampling-info)

	     ;; depth stencil
	     (null-pointer vk-pipeline-depth-stencil-state-create-info)
	     
	     (create-color-blending-info)

	     (null-pointer vk-pipeline-dynamic-state-create-info)

	     (pointer-ref-value (car layout))

	     (pointer-ref-value render-pass)

	     0
	     0
	     0))
	   (pipeline (make-foreign-object vk-pipeline)))
      (vk-create-graphics-pipelines device
				    0
				    1
				    pipeline-info
				    0
				    pipeline)
      (make-pipeline pipeline (car layout) render-pass (cdr layout)))))

;; samplte usage

(define device (vulkan-state-device vs))
(define swapchain-details (vulkan-state-swapchain vs))

(define vertices (list (make-vertex-input '#2( 0.0 -0.5) '#3(1.0 0.0 0.0))
		       (make-vertex-input '#2( 0.5  0.5) '#3(0.0 1.0 0.0))
		       (make-vertex-input '#2(-0.5  0.5) '#3(0.0 0.0 1.0))))

(define indices (list 0 1 2 2 3 0 4 5 6 6 7 4))

(define stride 4)

(define shaders (make-shaders "shaders/shader.vert" "shaders/shader.frag"))

(define pipeline-data
  (make-pipeline-data shaders
		      (make-vertex-input-details vertices
						 stride
						 (vertex-input->attrs (car vertices)))))

(define pipeline (create-graphics-pipeline device swapchain-details pipeline-data))
#!eof

=========================================================================================

(load "vulkan/pipeline.scm")
(define ss (create-shader-stages (vulkan-state-device vs)
				 (shaders-vertex shaders)
				 (shaders-fragment shaders)))

(ftype-pointer->sexpr (car ss))

(list->vk-pipeline-shader-stage-create-info-pointer-array ss)
(make-bytevector (*  4))
(bytevector-ieee-double-set!)

(define vertices-length (fold-left + 0 (map vertex-input-total-length vertices)))

(vertices->bytevector vertices)

(define attrs (vertex-input->attrs (car vertices)))

(define vertex-input-create-info (create-vertex-input stride attrs))

(define swapchain-extent (swapchain-details-extent (vulkan-state-swapchain vs)))

(create-graphics-pipeline device swapchain-details pipeline-data)

;; fix the list conversion

(list->vk-pipeline-shader-stage-create-info-pointer-array ss)

(vk-pipeline-shader-stage-create-info-pointer-map
 ftype-pointer->sexpr
 (list->vk-pipeline-shader-stage-create-info-pointer-array ss))
(load "vk.scm")
