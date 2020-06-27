(define-record-type vulkan-state
  (fields window surface physical-device queue-index device queues swapchain command-pool))

(define setup-vulkan
  (lambda ()
    (let* ((instance (init-vulkan))
	   (_ (glfw-init))
	   (window  (setup-window instance 3840 2160))
	   (surface (window-details-surface window))
	   (physical-device (array-pointer-raw-ptr (get-physical-devices instance)))
	   (queue-index (find-queue-family physical-device surface))
	   (device (create-logical-device physical-device queue-index))
	   (queues (create-queue-handles device))
	   (swapchain-details (create-swapchain physical-device device surface queue-index))
	   (command-pool (create-command-pool device queue-index)))
      (make-vulkan-state window surface physical-device queue-index device queues
			 swapchain-details command-pool))))

(define configure-descriptor-layout
  (lambda (device)

    (define fragement-sampler-binding
      (lambda (binding-index)
	(make-vk-descriptor-set-layout-binding binding-index
					       vk-descriptor-type-combined-image-sampler
					       1
					       vk-shader-stage-fragment-bit
					       (null-pointer vk-sampler))))

    (define uniform-buffer-binding
      (lambda (binding-index stage)
	(make-vk-descriptor-set-layout-binding binding-index
					       vk-descriptor-type-uniform-buffer
					       1
					       stage
					       (null-pointer vk-sampler))))
    
    (let* ((uniform-buffer-binding (uniform-buffer-binding 0 (bitwise-ior vk-shader-stage-vertex-bit
									  vk-shader-stage-fragment-bit)))
	   (light-info-binding (uniform-buffer-binding 1 vk-shader-stage-fragment-bit))
	   (texture-sampler-bindings (map fragment-sampler-binding (cddr (iota 10))))
	   (bindings (list->vk-descriptor-set-layout-binding-pointer-array
		      (append (list uniform-buffer-binding light-info-binding) bindings))))
      (create-descriptor-layout device bindings))))

;; returns a cons cell of (vertex-input-metadata . graphics-pipeline)
(define create-pipeline
  (lambda (state shaders model-filename)
    (match-let (((@ vulkan-state (physical-device physical-device)
				 (device device)
				 (swapchain swapchain)) state))
      (let* ((vertex-input-metadata (model->vertex-input-metadata model-filename))
	     (render-pass (configure-render-pass physical-device device swapchain))
	     (descriptor-layout (configure-descriptor-layout device))
	     (rasterization-state (make-rasterization-state vk-polygon-mode-fill
							    vk-cull-mode-back-bit
							    vk-front-face-clockwise))
	     (depth-state (make-depth-stencil-state #t #t vk-compare-op-less))
	     (pipeline-data (make-pipeline-data shaders
						(vertex-input->details vertex-input-metadata)
						render-pass
						descriptor-layout
						rasterization-state
						depth-state
						#t
						(swapchain-extent swapchain))))
	(cons (create-graphics-pipeline physical-device device pipeline-data)
	      vertex-input-metadata)))))

;; descriptor data



;; returns a list of ( command-pool command-buffers uniform-buffers)
(define create-buffers
  (lambda (state vertex-input-metadata-obj pipeline)
    (match-let* (((@ vulkan-state (physical-device physical-device)
				  (device device)
				  (swapchain swapchain-obj)
				  (queue-index queue-index)
				  (queues queues)
				  (command-pool command-pool)) state)
		 ((@ vertex-input-metadata (vertices-list vertices)
					   (indices indices)
					   (components components)) vertex-input-metadata-obj)
		 ((@ swapchain (image-views swapchain-image-views)
			       (extent extent)) swapchain-obj)
		 ((graphics-queue . present-queue) queues))
      (displayln "indices length " (length indices)
		 "vertices length" (length vertices))
      (let* ((vertex-buffer (create-gpu-local-buffer physical-device
						     device
						     graphics-queue
						     command-pool
						     vertices
						     vk-buffer-usage-vertex-buffer-bit))
	     (index-buffer (create-gpu-local-buffer physical-device
						    device
						    graphics-queue
						    command-pool
						    indices
						    vk-buffer-usage-index-buffer-bit))
	     (framebuffers (create-framebuffers-for-swapchain physical-device
							      device
							      command-pool
							      graphics-queue
							      swapchain-obj
							      pipeline))
	     (framebuffer-size (length framebuffers))
	     
	     (camera-uniform-buffers
	      (create-uniform-buffers physical-device
				      device
				      (uniform-buffer-data->list (extent->uniform-buffer-data extent))
				      framebuffer-size))
	     (lights-uniform-buffers
	      (create-uniform-buffers physical-device
				      device
				      (lights-data->list default-light-data)
				      framebuffer-size))
	     
	     (albedo-texture-data (create-texture-data physical-device
						       device
						       command-pool
						       graphics-queue
						       swapchain-obj
						       "textures/tile/ugznfcyo_4K_Albedo.jpg"))
	     (normal-texture-data (create-texture-data physical-device
						       device
						       command-pool
						       graphics-queue
						       swapchain-obj
						       "textures/tile/ugznfcyo_4K_Normal.jpg"))
	     (ao-texture-data (create-texture-data physical-device
						   device
						   command-pool
						   graphics-queue
						   swapchain-obj
						   "textures/tile/ugznfcyo_4K_AO.jpg"))
	     (metallic-texture-data (create-texture-data physical-device
							 device
							 command-pool
							 graphics-queue
							 swapchain-obj
							 "textures/tile/ugznfcyo_4K_Gloss.jpg"))

	     (roughness-texture-data (create-texture-data physical-device
							  device
							  command-pool
							  graphics-queue
							  swapchain-obj
							  "textures/tile/ugznfcyo_4K_Roughness.jpg"))
	     
	     (descriptor-sets (create-descriptor-sets device
						      (create-descriptor-pool device
									      framebuffer-size)
						      (pipeline-descriptor-set-layout pipeline)
						      (map cons
							   camera-uniform-buffers
							   lights-uniform-buffers)
						      (list albedo-texture-data
							    normal-texture-data
							    ao-texture-data
							    metallic-texture-data
							    roughness-texture-data))))
	(cons (create-command-buffers device
				      swapchain-obj
				      command-pool
				      pipeline
				      vertex-buffer
				      index-buffer
				      framebuffers
				      descriptor-sets
				      (length indices)
				      components)
	      uniform-buffers)))))

(define sync-objects (lambda (device) (init-sync-objects device)))

(define initial-state
  (lambda (cmd-buffers)
    (make-frame-state 1
		      (list->vector (map (lambda (_) #f)
					 (iota (array-pointer-length cmd-buffers)))))))


(define run-draw-loop
  (lambda (state uniform-buffers command-buffers)
    (match state
      ((@ vulkan-state (window window)
		       (device device)
		       (swapchain swapchain)
		       (queues queues))
       (draw-next-frame (window-details-window window)
			device
			swapchain
			queues
			uniform-buffers
			command-buffers
			(sync-objects device)
			(initial-state command-buffers))))))



;;; (load "vulkan.scm")
