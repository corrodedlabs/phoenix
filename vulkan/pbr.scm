;; BRDF integration map

(define state (setup-vulkan))
(define device (vulkan-state-device state))
(define physical-device (vulkan-state-physical-device state))

;; brdf material pipeline

(define +dimension+ 512)
(define +format+ vk-format-r16g16-sfloat)

(define create-image
  (lambda ()
    (let* ((image-property (create-image-property-for-texture +dimension+
							      +dimension+
							      +format+))
	   (image-handle (create-image-handle device image-property))
	   (image-memory (allocate-memory-for-image physical-device device image-handle)))
      (make-gpu-image image-handle
		      (create-image-view device image-handle image-property)
		      image-memory))))

(define create-pbr-texture
  (lambda ()
    (let ((image (create-image))
	  (sampler (create-texture-sampler device
					   (make-texture-property
					    vk-sampler-address-mode-clamp-to-edge
					    0.0
					    1.0
					    vk-border-color-float-opaque-white))))
      (make-texture-data (gpu-image-view image) sampler))))


(define brdf-render-pass
  (lambda ()
    
    ;; subpass dependency for layout transition
    (define subpass-dependencies
      (list
       (make-vk-subpass-dependency vk-subpass-external
				   0
				   vk-pipeline-stage-bottom-of-pipe-bit
				   vk-pipeline-stage-color-attachment-output-bit
				   vk-access-memory-read-bit
				   (bitwise-ior vk-access-color-attachment-read-bit
						vk-access-color-attachment-write-bit)
				   vk-dependency-by-region-bit)
       (make-vk-subpass-dependency 0
				   vk-subpass-external
				   vk-pipeline-stage-color-attachment-output-bit
				   vk-pipeline-stage-bottom-of-pipe-bit
				   (bitwise-ior vk-access-color-attachment-read-bit
						vk-access-color-attachment-write-bit)
				   vk-access-memory-read-bit
				   vk-dependency-by-region-bit)))

    (let* ((attachment-description
	    (create-color-attachment-description +format+
						 vk-image-layout-shader-read-only-optimal))
	   (color-attachment-ref (optimal-color-attachment-reference))

	   (subpass-description (create-subpass-description color-attachment-ref)))
      (create-render-pass device
			  (make-array-pointer 1
					      attachment-description
					      'vk-attachment-description)
			  (make-array-pointer 1 subpass-description 'vk-subpass-description)
			  (list->vk-subpass-dependency-pointer-array subpass-dependencies)))))


(define pbr-texture-data (create-pbr-texture))

(define generate-brdf-pipeline
  (lambda ()
    (let ((render-pass (brdf-render-pass))
	  (descriptor-layout
	   (create-descriptor-layout device
				     (make-array-pointer 0
							 (null-pointer
							  vk-descriptor-set-layout-binding)
							 'vk-descriptor-set-layout-binding)))
	  (rasterization-state (make-rasterization-state vk-polygon-mode-fill
							 vk-cull-mode-none
							 vk-front-face-clockwise))
	  (depth-stencil-state (make-depth-stencil-state #f #f vk-compare-op-less-or-equal)))
      (create-graphics-pipeline physical-device device
				(make-pipeline-data (make-shaders "shaders/brdf.vert"
								  "shaders/brdf.frag")
						    (make-vertex-input-details ''() 0 '())
						    render-pass
						    descriptor-layout
						    rasterization-state
						    depth-stencil-state
						    (make-vk-extent-2d +dimension+ +dimension+))))))

(define brdf-pipeline (generate-brdf-pipeline))
