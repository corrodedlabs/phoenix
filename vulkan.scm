#!chezscheme
(library (vulkan)

  (export setup-vulkan
	  make-shaders
	  create-pipeline
	  create-buffers
	  run-draw-loop)

  (import (chezscheme)
	  (prelude)
	  (ffi)
	  (glfw)
	  (assimp)
	  (glfw)
	  (shaderc)
	  (camera)
	  (image)
	  (matchable)
	  
	  (vulkan structure-types))

  (define v (load-shared-object "libvulkan.so.1"))

  (include "vulkan/enums.scm")
  (include "vulkan/ftype.scm")

  (include "vulkan/instance.scm")
  (include "vulkan/surface.scm")

  (include "vulkan/queues.scm")
  (include "vulkan/devices.scm")
  (include "vulkan/swapchain.scm")

  (include "vulkan/images.scm")
  (include "vulkan/pipeline.scm")
  (include "vulkan/mesh.scm")
  (include "vulkan/texture.scm")
  (include "vulkan/buffers.scm")

  (include "vulkan/sync.scm")

  (define-record-type vulkan-state
    (fields window surface physical-device queue-index device queues swapchain))

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
	     (swapchain-details (create-swapchain physical-device device surface queue-index)))
	(make-vulkan-state window surface physical-device queue-index device queues
			   swapchain-details))))

  ;; returns a cons cell of (vertex-input-metadata . graphics-pipeline)
  (define create-pipeline
    (lambda (state shaders model-filename)
      (match-let (((@ vulkan-state (physical-device physical-device)
				   (device device)
				   (swapchain swapchain)) state))
	(let* ((vertex-input-metadata (model->vertex-input-metadata model-filename))
	       (pipeline-data (make-pipeline-data shaders
						  (vertex-input->details vertex-input-metadata))))
	  (cons vertex-input-metadata
		(create-graphics-pipeline physical-device device swapchain pipeline-data))))))

  ;; returns a cons cell of (uniform-buffers . command-buffers)
  (define create-buffers
    (lambda (state vertex-input-metadata-obj pipeline)
      (match-let* (((@ vulkan-state (physical-device physical-device)
				    (device device)
				    (swapchain swapchain-obj)
				    (queue-index queue-index)
				    (queues queues)) state)
		   ((@ vertex-input-metadata (vertices-list vertices)
					     (indices indices)
					     (components components)) vertex-input-metadata-obj)
		   ((@ swapchain (image-views swapchain-image-views)
				 (extent extent)) swapchain-obj)
		   ((graphics-queue . present-queue) queues))
	(displayln "indices length " (length indices)
		   "vertices length" (length vertices))
	(let* ((command-pool (create-command-pool device queue-index))
	       (vertex-buffer (create-gpu-local-buffer physical-device
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
	       (framebuffers (create-framebuffers physical-device
						  device
						  command-pool
						  graphics-queue
						  swapchain-obj
						  pipeline))
	       (framebuffer-size (length framebuffers))
	       (uniform-buffers (create-uniform-buffers physical-device
							device
							(extent->uniform-buffer-data extent)
							framebuffer-size))
	       (texture-data (create-texture-data physical-device
						  device
						  command-pool
						  graphics-queue
						  swapchain-obj))
	       (descriptor-sets (create-descriptor-sets device
							(create-descriptor-pool device
										framebuffer-size)
							(pipeline-descriptor-set-layout pipeline)
							uniform-buffers
							texture-data)))
	  (cons uniform-buffers (create-command-buffers device
							swapchain-obj
							command-pool
							pipeline
							vertex-buffer
							index-buffer
							framebuffers
							descriptor-sets
							(length indices)
							components))))))

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
			  (initial-state command-buffers)))))))


;;; (load "vulkan.scm")
