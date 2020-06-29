;; BRDF integration map

;; (define-record-type vulkan-data (fields ))

;; brdf material pipeline
(define brdf-data
  (lambda (physical-device device command-pool graphics-queue)
    (define +dimension+ 512)
    (define +format+ vk-format-r16g16-sfloat)

    (define +dimension-extent+ (make-vk-extent-2d +dimension+ +dimension+))

    (define create-image
      (lambda ()
	(let* ((image-property
		(create-image-property-for-texture +dimension+
						   +dimension+
						   +format+
						   vk-image-usage-color-attachment-bit))
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
	       (color-attachment-ref
		(create-color-attachment-reference vk-image-layout-color-attachment-optimal))

	       (subpass-description (create-subpass-description color-attachment-ref)))
	  (create-render-pass device
			      (make-array-pointer 1
						  attachment-description
						  'vk-attachment-description)
			      (make-array-pointer 1 subpass-description 'vk-subpass-description)
			      (list->vk-subpass-dependency-pointer-array subpass-dependencies)))))


    (define generate-brdf-pipeline
      (lambda (render-pass)
	(let ((descriptor-layout
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
							(make-vertex-input-details '() 0 '())
							render-pass
							descriptor-layout
							rasterization-state
							depth-stencil-state
							+dimension-extent+)))))

    (define clear-values (list 0.0 0.0 0.0 1.0))
    (define clear-values-ptr (list->vk-clear-value-pointer-array
			      (list (make-vk-clear-value clear-values))))

    (define execute-brdf-pass
      (lambda (pbr-texture-data render-pass brdf-pipeline framebuffer)
	
	(define render-pass
	  (lambda (command-buffer)
	    (let ((render-area (make-render-area '(0 . 0) (cons +dimension+ +dimension+))))
	      (perform-render-pass (make-render-pass-data command-buffer
							  framebuffer
							  render-area
							  clear-values-ptr
							  brdf-pipeline)
				   (lambda () (vk-cmd-draw command-buffer 3 1 0 0))))))
	
	(execute-command-buffer device command-pool graphics-queue #t render-pass)))

    (displayln "executing brdf pipeline")
    (let* ((pbr-texture-data (create-pbr-texture))
	   (render-pass (brdf-render-pass))
	   (brdf-pipeline (generate-brdf-pipeline render-pass))
	   (framebuffer (create-framebuffer device
					    render-pass
					    (make-vk-extent-2d +dimension+ +dimension+)
					    (texture-data-image-view pbr-texture-data))))
      (execute-brdf-pass pbr-texture-data render-pass brdf-pipeline framebuffer)
      pbr-texture-data)))
