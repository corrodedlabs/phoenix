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



;; let's capture all the custom information that can be supplied in the pipeline in a record

;; vertex input

;; this will contain data about mesh component in a model
(define-record-type mesh-component (fields index-count index-base))

;; components is a array of #<mesh-component>
(define-record-type vertex-input-metadata
  (fields size vertices-list stride indices attrs components))

;; default vertex input struct
(define-record-type vertex-input (fields position color texture-coord))



;; currently supported shaders
(define-record-type shaders (fields vertex fragment))
(define-record-type vertex-input-details (fields vertex-input-list stride attrs))
(define-record-type rasterization-state (fields polygon-mode cull-mode front-face))
(define-record-type depth-stencil-state (fields depth-test? depth-write? depth-compare-op))

(define-record-type pipeline-data
  (fields shaders vertex-input-details render-pass descriptor-layout rasterization-state
	  depth-stencil-state viewport))

;; takes input a nested list and returns a list of cons cell (format . offset)
;; these are usually saved in the field attrs of vertex-input-metadata
(define vector->attr
  (lambda (input)
    (displayln "input is " input)
    (let lp ((elems input)
	     (offset 0)
	     (attrs (list)))
      
      (cond
       ((null? elems) (reverse attrs))
       (else
	(case (cond
	       ((vector? (car elems)) (vector-length (car elems)))
	       ((list? (car elems)) (length (car elems))))
	  ((2) (lp (cdr elems)
		   (fx+ offset (* 2 4))
		   (cons (cons vk-format-r32g32-sfloat offset) attrs)))
	  ((3) (lp (cdr elems)
		   (fx+ offset (* 3 4))
		   (cons (cons vk-format-r32g32b32-sfloat offset) attrs)))
	  (else (error "unsupported vector" v))))))))

(define create-vertex-input-metadata
  (lambda (vertices-list indices-list)
    
    (define vertex-input-total-length
      (lambda (input)
	(fx+ (vector-length (vertex-input-position input))
	     (vector-length (vertex-input-color input))
	     (vector-length (vertex-input-texture-coord input)))))

    ;; assuming that we are using float for everything
    (define vertex-input-total-size
      (lambda (input) (fx* 4 (vertex-input-total-length input))))


    (define (sizeof-vertex-input arr)
      (fold-left + 0 (map vertex-input-total-size arr)))

    (define vertex-input->list
      (lambda (vertices)
	(let ((vertices-list (map (lambda (v)
				    (match v
				      (($ vertex-input pos color tex-coord)
				       (list pos color tex-coord))))
				  vertices)))
	  (let lp ((v vertices-list)
		   (flat-coll (list)))
	    (displayln "v" v)
	    (cond
	     ((null? v) flat-coll)
	     (else (lp (cdr v)
		       (append flat-coll (apply append
						(map (lambda (v) (vector->list v))
						     (car v)))))))))))

    (define vertex-input-stride
      (lambda (vertices)
	(fx* 4 (vertex-input-total-length (car vertices)))))

    (make-vertex-input-metadata (sizeof-vertex-input vertices-list)
				(vertex-input->list vertices-list)
				(vertex-input-stride vertices-list)
				indices-list
				(match (car vertices-list)
				  (($ vertex-input pos color tex-coord)
				   (vector->attr (list pos color tex-coord)))))))


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
	(displayln "attrs is " attrs)
	(array-pointer-raw-ptr
	 (list->vk-vertex-input-attribute-description-pointer-array
	  (map-indexed (lambda (attr i)
			 (displayln "attr" i)
			 (make-vk-vertex-input-attribute-description i
								     0
								     (car attr)
								     (cdr attr)))
		       attrs)))))
    
    (let ((binding-count (if (null? attrs) 0 1))
	  (binding-description (if (null? attrs)
				   (null-pointer vk-vertex-input-binding-description)
				   (make-vertex-binding-description))))
      (make-vk-pipeline-vertex-input-state-create-info pipeline-vertex-input-state-create-info
						       0
						       0
						       binding-count
						       binding-description
						       (length attrs)
						       (make-attribute-descriptions)))))


;; input assembly

(define create-pipeline-assembly-state
  (lambda ()
    (make-vk-pipeline-input-assembly-state-create-info
     pipeline-input-assembly-state-create-info 0 0
     vk-primitive-topology-triangle-list
     vk-false)))

;; viewport

(define create-viewport-info
  (case-lambda
   (() (create-viewport-info (make-vk-extent-2d 0 0)))
   ((swapchain-extent)

    (define (create-viewport)
      (let ((width (vk-extent-2d-width swapchain-extent))
	    (height (vk-extent-2d-height swapchain-extent)))
	(make-vk-viewport 0.0
			  0.0
			  (exact->inexact width)
			  (exact->inexact height)
			  0.0
			  (if (and (equal? 0 width)
				 (equal? 0 height)) 0.0 1.0))))

    (define (create-scissor)
      (make-vk-rect-2d (make-vk-offset-2d 0 0) swapchain-extent))

    (make-vk-pipeline-viewport-state-create-info pipeline-viewport-state-create-info 0 0
						 1
						 (create-viewport)
						 1
						 (create-scissor)))))

;; rasterizer

(define (create-rasterizer-info rasterization-state)
  (match rasterization-state
    (($ rasterization-state polygon-mode cull-mode front-face)
     (make-vk-pipeline-rasterization-state-create-info pipeline-rasterization-state-create-info
						       0
						       0
						       vk-false
						       vk-false
						       polygon-mode
						       cull-mode
						       front-face
						       vk-false
						       0.0
						       0.0
						       0.0
						       1.0))))

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


(define-record-type descriptor-layout-detail
  (fields descriptor-type shader-stage immutable-sampler descriptor-count))

;; descriptor set layout

(define create-descriptor-layout
  (lambda (device bindings)
    (let ((info (make-vk-descriptor-set-layout-create-info descriptor-set-layout-create-info
							   0
							   0
							   (array-pointer-length bindings)
							   (array-pointer-raw-ptr bindings)))
	  (layout (make-foreign-object vk-descriptor-set-layout)))
      (vk-create-descriptor-set-layout device info 0 layout)
      layout)))

(define descriptor-layout-details->descriptor-layout
  (lambda (device details)

    (define details->bindings
      (lambda ()
	(list->vk-descriptor-set-layout-binding-pointer-array
	 (map-indexed (lambda (detail index)
			(match detail
			  (($ descriptor-layout-detail
			      descriptor-type
			      shader-stage
			      sampler
			      descriptor-count)
			   (let ((sampler (or sampler (null-pointer vk-sampler))))
			     (make-vk-descriptor-set-layout-binding index
								    descriptor-type
								    descriptor-count
								    shader-stage
								    sampler)))))
		      details))))
    
    (create-descriptor-layout device (details->bindings))))



;; pipeline layout

;; returns (pipeline-layout . descriptor-layout)
(define (create-pipeline-layout device descriptor-layout)
  (let* ((descriptor-layout (or descriptor-layout
			       (error "no descriptor layout" #f)))
	 (layout-info
	  (make-vk-pipeline-layout-create-info pipeline-layout-create-info 0 0
					       1
					       descriptor-layout
					       0
					       (null-pointer vk-push-constant-range)))
	 (layout (make-foreign-object vk-pipeline-layout)))
    (vk-create-pipeline-layout device layout-info 0 layout)
    (cons layout descriptor-layout)))

(define vertex-input->details
  (lambda (input-metadata)
    (match input-metadata
      ((@ vertex-input-metadata (vertices-list v) (stride s) (attrs a))
       (make-vertex-input-details v s a)))))

;; render pass

(define create-color-attachment-description
  (lambda (format final-layout)
    (make-vk-attachment-description 0
				    format
				    vk-sample-count-1-bit ;; samples
				    vk-attachment-load-op-clear ;; load-op
				    vk-attachment-store-op-store ;; store-op
				    vk-attachment-load-op-dont-care ;;stencil-load-op
				    vk-attachment-store-op-dont-care
				    vk-image-layout-undefined
				    final-layout)))

(define create-color-attachment-reference
  (lambda (final-layout) (make-vk-attachment-reference 0 final-layout)))

(define create-subpass-description
  (case-lambda
   ((color-attachment-ref) (create-subpass-description color-attachment-ref #f))
   ((color-attachment-ref depth-attachment-ref)
    (make-vk-subpass-description 0
				 vk-pipeline-bind-point-graphics
				 0
				 (null-pointer vk-attachment-reference)
				 1
				 color-attachment-ref
				 (null-pointer vk-attachment-reference)
				 (or depth-attachment-ref
				    (null-pointer vk-attachment-reference))
				 0
				 (null-pointer unsigned-32)))))

;; defined as (~0U)
(define vk-subpass-external 0)

(define create-render-pass
  (lambda (device attachments subpasses dependencies)
    (let ((info (make-vk-render-pass-create-info render-pass-create-info 0 0
						 (array-pointer-length attachments)
						 (array-pointer-raw-ptr attachments)
						 (array-pointer-length subpasses)
						 (array-pointer-raw-ptr subpasses)
						 (array-pointer-length dependencies)
						 (array-pointer-raw-ptr dependencies)))
	  (render-pass (make-foreign-object vk-render-pass)))
      (vk-create-render-pass device info 0 render-pass)
      render-pass)))

(define configure-render-pass
  (lambda (physical-device device swapchain)
    
    (define create-render-pass-info
      (lambda ()
	(let* ((swapchain-image-format
		(vk-surface-format-khr-format (swapchain-format swapchain)))
	       
	       (color-attachment
		(create-color-attachment-description swapchain-image-format
						     vk-image-layout-present-src-khr))
	       
	       (color-attachment-ref (create-color-attachment-reference
				      vk-image-layout-color-attachment-optimal))
	       
	       (depth-attachment
		(make-vk-attachment-description 0
						(find-depth-format physical-device)
						vk-sample-count-1-bit
						vk-attachment-load-op-clear
						vk-attachment-store-op-dont-care
						vk-attachment-load-op-dont-care
						vk-attachment-store-op-dont-care
						vk-image-layout-undefined
						vk-image-layout-depth-stencil-attachment-optimal))

	       (depth-attachment-ref
		(make-vk-attachment-reference 1
					      vk-image-layout-depth-stencil-attachment-optimal))

	       (attachments (list->vk-attachment-description-pointer-array
			     (list color-attachment depth-attachment)))
	       
	       (subpass (create-subpass-description color-attachment-ref depth-attachment-ref))
	       
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
					   (array-pointer-length attachments)
					   (array-pointer-raw-ptr attachments)
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

(define create-depth-stencil-state
  (lambda (depth-stencil)
    (define bool->vulkan (lambda (x) (if x vk-true vk-false)))
    
    (match depth-stencil
      (($ depth-stencil-state depth-test? depth-write? depth-compare-op)
       (let ((zero-op-state (make-vk-stencil-op-state 0 0 0 0 0 0)))
	 (make-vk-pipeline-depth-stencil-state-create-info pipeline-depth-stencil-state-create-info
							   0
							   0
							   (bool->vulkan depth-test?)
							   (bool->vulkan depth-write?)
							   depth-compare-op
							   vk-false
							   vk-false
							   zero-op-state
							   zero-op-state
							   0.0
							   1.0))))))



(define create-graphics-pipeline
  (lambda (physical-device device pipeline-data)
    (let* ((shaders (pipeline-data-shaders pipeline-data))

	   (shader-stages (list->vk-pipeline-shader-stage-create-info-pointer-array
			   (create-shader-stages device
						 (shaders-vertex shaders)
						 (shaders-fragment shaders))))

	   (vertex-input-data (pipeline-data-vertex-input-details pipeline-data))

	   (render-pass (pipeline-data-render-pass pipeline-data))
	   (layout (create-pipeline-layout device (pipeline-data-descriptor-layout pipeline-data)))

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
	     (null-pointer vk-pipeline-tessellation-state-create-info)

	     ;; viewport
	     (create-viewport-info (pipeline-data-viewport pipeline-data))

	     ;; rasterizer
	     (create-rasterizer-info (pipeline-data-rasterization-state pipeline-data))

	     ;; multisampling
	     (create-multisampling-info)

	     ;; depth stencil
	     (create-depth-stencil-state (pipeline-data-depth-stencil-state pipeline-data))
	     
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

;; (load "vulkan/mesh.scm")

;; samplte usage

;; (define device (vulkan-state-device vs))
;; (define physical-device (vulkan-state-physical-device vs))
;; (define swapchain-details (vulkan-state-swapchain vs))

;; (define vertices (list (make-vertex-input '#3( -0.5 -0.5 0.0) '#3(1.0 0.0 0.0) '#2(1.0 0.0))
;; 		       (make-vertex-input '#3( 0.5  -0.5 0.0) '#3(0.0 1.0 0.0) '#2(0.0 0.0))
;; 		       (make-vertex-input '#3( 0.5   0.5 0.0) '#3(0.0 0.0 1.0) '#2(0.0 1.0))
;; 		       (make-vertex-input '#3(-0.5   0.5 0.0) '#3(1.0 1.0 1.0) '#2(1.0 1.0))

;; 		       (make-vertex-input '#3( -0.5 -0.5 -0.5) '#3(1.0 0.0 0.0) '#2(1.0 0.0))
;; 		       (make-vertex-input '#3( 0.5  -0.5 -0.5) '#3(0.0 1.0 0.0) '#2(0.0 0.0))
;; 		       (make-vertex-input '#3( 0.5   0.5 -0.5) '#3(0.0 0.0 1.0) '#2(0.0 1.0))
;; 		       (make-vertex-input '#3(-0.5   0.5 -0.5) '#3(1.0 1.0 1.0) '#2(1.0 1.0))))

;; (define indices (list 0 1 2 2 3 0
;; 		      4 5 6 6 7 4))

;; (define shaders (make-shaders "shaders/shader.vert" "shaders/shader.frag"))


;; (define vertex-input-metadata (model->vertex-input-metadata "models/box.obj"))

;; (define pipeline-data
;;   (make-pipeline-data shaders (vertex-input->details vertex-input-metadata)))

;; (define pipeline
;;   (create-graphics-pipeline physical-device device swapchain-details pipeline-data))
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
