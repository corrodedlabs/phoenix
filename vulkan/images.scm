
;; Images

;; for swapchain these are created automatically
;; for textures and depth buffers, these have to be created manually


;; find supported formats out of candidate format with the given
;; tiling and features
(define find-supported-formats
  (lambda (physical-device candidate-formats tiling features)

    (define get-format-properties
      (lambda (format)
	(let ((format-properties (make-foreign-object vk-format-properties)))
	  (vk-get-physical-device-format-properties physical-device
						    format
						    format-properties)
	  format-properties)))
    
    (find (lambda (format)
	    (let ((props (get-format-properties format)))
	      (or (and (equal? tiling vk-image-tiling-linear)
		    (equal? features (bitwise-and
				      (vk-format-properties-linear-tiling-features props)
				      features)))
		 (and (equal? tiling vk-image-tiling-optimal)
		    (equal? features (bitwise-and
				      (vk-format-properties-optimal-tiling-features props)
				      features))))))
	  candidate-formats)))

;; There are several formats that fit this requirement:

;; VK_FORMAT_D32_SFLOAT: 32-bit float for depth
;; VK_FORMAT_D32_SFLOAT_S8_UINT: 32-bit signed float for depth and 8 bit stencil component
;; VK_FORMAT_D24_UNORM_S8_UINT: 24-bit float for depth and 8 bit stencil component
;;
(define candidate-formats (list vk-format-d32-sfloat
				vk-format-d32-sfloat-s8-uint
				vk-format-d24-unorm-s8-uint))

;; Image Properties required to create a image
(define-record-type image-properties
  (nongenerative)
  (fields width height format usage-flags aspect-flags))

(define create-image-handle
  (lambda (device image-properties)
    (let* ((extent (make-vk-extent-3d (image-properties-width image-properties)
				      (image-properties-height image-properties)
				      1))
	   (info
	    (make-vk-image-create-info image-create-info
				       0
				       0
				       vk-image-type-2d
				       (image-properties-format image-properties)
				       extent
				       1
				       1
				       vk-sample-count-1-bit
				       vk-image-tiling-optimal
				       (image-properties-usage-flags image-properties)
				       vk-sharing-mode-exclusive
				       0
				       (null-pointer u32)
				       vk-image-layout-undefined))
	   (image (make-foreign-object vk-image)))
      (vk-create-image device info 0 image)
      image)))


(define create-depth-property
  (lambda (extent format)
    (make-image-properties (vk-extent-2d-width extent)
			   (vk-extent-2d-height extent)
			   format
			   vk-image-usage-depth-stencil-attachment-bit
			   vk-image-aspect-depth-bit)))

(define create-texture-property
  (lambda (texture-width texture-height)
    (make-image-properties texture-width
			   texture-height
			   vk-format-r8g8b8a8-unorm
			   (bitwise-ior vk-image-usage-transfer-dst-bit
					vk-image-usage-sampled-bit)
			   vk-image-aspect-color-bit)))

(define allocate-image-memory
  (lambda (physical-device device image-handle)

    (define (memory-requirements)
      (let ((memory-requirements (make-foreign-object vk-memory-requirements)))
	(vk-get-image-memory-requirements device image-handle memory-requirements)
	memory-requirements))

    (let* ((requirements (memory-requirements))
	   (alloc-info (make-vk-memory-allocate-info
			memory-allocate-info
			0
			(vk-memory-requirements-size requirements)
			(find-memory-type-index physical-device
						(vk-memory-requirements-memory-type-bits
						 requirements)
						vk-memory-property-device-local-bit)))
	   (memory (make-foreign-object vk-device-memory)))
      (vk-allocate-memory device alloc-info 0 memory)
      memory)))

(define bind-image-memory
  (lambda (device image-handle memory)
    (vk-bind-image-memory device image-handle memory 0)))


;; Transition barriers

;; uses command buffers to move images to different stages

(define-record-type transition-barrier-info
  (nongenerative)
  (fields src-access-mask src-stage dst-access-mask dst-stage))

;; returns barrier info for performing the transition from 'old-layout' to 'new-layout'
(define layout-transition->barrier-info
  (lambda (old-layout new-layout)
    (cond 
     ((equal? old-layout vk-image-layout-undefined)
      (cond 
       ((equal? new-layout vk-image-layout-transfer-dst-optimal)
	(make-transition-barrier-info 0
				      vk-pipeline-stage-top-of-pipe-bit
				      vk-access-transfer-write-bit
				      vk-pipeline-stage-transfer-bit))

       ((equal? new-layout vk-image-layout-depth-stencil-attachment-optimal)
	(make-transition-barrier-info 0
				      vk-pipeline-stage-top-of-pipe-bit
				      (bitwise-ior
				       vk-access-depth-stencil-attachment-read-bit
				       vk-access-depth-stencil-attachment-write-bit)
				      vk-pipeline-stage-early-fragment-tests-bit))

       ((equal? new-layout vk-image-layout-color-attachment-optimal)
	(make-transition-barrier-info 0
				      vk-pipeline-stage-top-of-pipe-bit
				      (bitwise-ior vk-access-color-attachment-read-bit
						   vk-access-color-attachment-write-bit)
				      vk-pipeline-stage-color-attachment-output-bit))
       
       (else (error "unsupported new-layout in " (list old-layout "=>" new-layout)))))

     ((and (equal? old-layout vk-image-layout-transfer-dst-optimal)
	 (and (equal? new-layout vk-image-layout-shader-read-only-optimal)))
      (make-transition-barrier-info vk-access-transfer-write-bit
				    vk-pipeline-stage-transfer-bit
				    vk-access-shader-read-bit
				    vk-pipeline-stage-fragment-shader-bit))

     (else (error "unsupported transition " (list old-layout "=>" new-layout))))))


(define stencil-component-present?
  (lambda (format)
    (or (equal? format vk-format-d32-sfloat-s8-uint)
       (equal? format vk-format-d24-unorm-s8-uint))))

;; generic function to handle image transitions
(define transition-image-layout
  (lambda (device command-pool graphics-queue format image-handle old-layout new-layout)
    (match (layout-transition->barrier-info old-layout new-layout)
      (($ transition-barrier-info
	  src-access-mask src-stage dst-access-mask dst-stage)
       (let* ((aspect-mask (cond
			    ((and (equal? new-layout
					vk-image-layout-depth-stencil-attachment-optimal)
				(stencil-component-present? format))
			     (bitwise-ior vk-image-aspect-depth-bit
					  vk-image-aspect-stencil-bit))

			    ((equal? new-layout
				     vk-image-layout-depth-stencil-attachment-optimal)
			     vk-image-aspect-depth-bit)

			    (else vk-image-aspect-color-bit)))
	      (subresource-range
	       (make-vk-image-subresource-range aspect-mask
						0
						1 ;;miplevels <level count>
						0
						1))
	      (image-barrier (make-vk-image-memory-barrier image-memory-barrier
							   0
							   src-access-mask
							   dst-access-mask
							   old-layout
							   new-layout
							   0
							   0
							   (pointer-ref-value image-handle)
							   subresource-range)))
	 (execute-command-buffer device
				 command-pool
				 graphics-queue
				 (lambda (command-buffer)
				   (vk-cmd-pipeline-barrier command-buffer
							    src-stage
							    dst-stage
							    0
							    0
							    0
							    0
							    0
							    1
							    image-barrier)))))
      
      (else (error "could not transition to " new-layout)))))

;; image view

(define create-image-view
  (lambda (device image-handle image-property)
    (match image-property
      ((@ image-properties (format f) (aspect-flags aspect))
       (let* ((component-mapping (make-vk-component-mapping vk-component-swizzle-identity
							    vk-component-swizzle-identity
							    vk-component-swizzle-identity
							    vk-component-swizzle-identity))
	      (subresource-range (make-vk-image-subresource-range aspect 0 1 0 1))
	      (info (make-vk-image-view-create-info image-view-create-info
						    0
						    0
						    (pointer-ref-value image-handle)
						    vk-image-view-type-2d
						    f
						    component-mapping
						    subresource-range))
	      (image-view (make-foreign-object vk-image-view)))
	 (vk-create-image-view device info 0 image-view)
	 image-view)))))

;; record to represent an image in a gpu
(define-record-type gpu-image
  (nongenerative)
  (fields handle view memory))

(define find-depth-format
  (lambda (physical-device)
    (find-supported-formats physical-device
			    candidate-formats
			    vk-image-tiling-optimal
			    vk-format-feature-depth-stencil-attachment-bit)))

(define create-depth-buffer-image
  (lambda (physical-device device command-pool graphics-queue swapchain)

    (define create-gpu-image
      (lambda (property)
	(let* ((image-handle (create-image-handle device property))
	       (memory (allocate-image-memory physical-device device image-handle)))
	  (bind-image-memory device image-handle memory)
	  (transition-image-layout device
				   command-pool
				   graphics-queue
				   (image-properties-format property)
				   image-handle
				   vk-image-layout-undefined
				   vk-image-layout-depth-stencil-attachment-optimal)
	  (make-gpu-image image-handle
			  (create-image-view device image-handle property)
			  memory))))
    
    (let* ((extent (swapchain-extent swapchain))
	   (supported-format (find-depth-format physical-device))
	   (depth-property (create-depth-property extent supported-format)))
      (create-gpu-image depth-property))))


(define create-texture-image
  (lambda (physical-device device command-pool graphics-queue swapchain texture-path)

    (define copy-buffer-to-image
      (lambda (buffer image width height)
	(let* ((subresource (make-vk-image-subresource-layers vk-image-aspect-color-bit 0 0 1))
	       (offset (make-vk-offset-3d 0 0 0))
	       (extent (make-vk-extent-3d width height 1))
	       (region (make-vk-buffer-image-copy 0 0 0 subresource offset extent)))
	  (execute-command-buffer device
				  command-pool
				  graphics-queue
				  (lambda (command-buffer)
				    (vk-cmd-copy-buffer-to-image command-buffer
								 buffer
								 image
								 vk-image-layout-transfer-dst-optimal
								 1
								 region))))))

    (define create-image-for-texture
      (lambda (image-buffer image-data)
	(let* ((width (image-data-width image-data))
	       (height (image-data-height image-data))
	       (property (create-texture-property width height))
	       (image-handle (create-image-handle device property))
	       (memory (allocate-image-memory physical-device device image-handle)))
	  (bind-image-memory device image-handle memory)
	  (transition-image-layout device
				   command-pool
				   graphics-queue
				   vk-format-r8g8b8a8-unorm
				   image-handle
				   vk-image-layout-undefined
				   vk-image-layout-transfer-dst-optimal)
	  (copy-buffer-to-image image-buffer image-handle width height)
	  (transition-image-layout device
				   command-pool
				   graphics-queue
				   vk-format-r8g8b8a8-unorm
				   image-handle
				   vk-image-layout-transfer-dst-optimal
				   vk-image-layout-shader-read-only-optimal)
	  (cons (make-gpu-image image-handle #f memory) property))))

    (define create-texture-image-handle
      (lambda (texture-path)
	(with-image texture-path
		    (lambda (image)
		      (let ((buffer (create-host-buffer physical-device
							device
							(make-heap-data (image-data-pointer image)
									(image-data-size image)))))
			(create-image-for-texture (buffer-handle buffer) image))))))


    (match (create-texture-image-handle texture-path)
      ((image . property)
       (match image
	 (($ gpu-image handle _ memory)
	  (make-gpu-image handle (create-image-view device handle property) memory)))))))


#!eof
;; Sample usage

(define extent (swapchain-extent swapchain))

(define depth-property (create-depth-property extent supported-format))

(define depth-image-handle (create-image-handle device depth-property))

(define memory (allocate-image-memory physical-device device depth-image-handle ))

(bind-image-memory device depth-image-handle memory)

(layout-transition->barrier-info vk-image-layout-undefined
				 vk-image-layout-depth-stencil-attachment-optimal)

(transition-image-layout device
			 command-pool
			 graphics-queue
			 supported-format
			 depth-image-handle
			 vk-image-layout-undefined
			 vk-image-layout-depth-stencil-attachment-optimal)

(create-image-view device depth-image-handle depth-property)
