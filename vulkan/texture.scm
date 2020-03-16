
(define texture-path "textures/winter.jpeg")

(define copy-buffer-to-image
  (lambda (device command-pool graphics-queue buffer image width height)
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
  (lambda (physical-device device command-pool graphics-queue swapchain image-buffer image-data)
    (let* ((width (image-data-width image-data))
	   (height (image-data-height image-data))
	   (image-handle (create-image-handle device (create-texture-property width height)))
	   (memory (allocate-image-memory physical-device device image-handle)))
      (bind-image-memory device image-handle memory)
      (transition-image-layout device
			       command-pool
			       graphics-queue
			       vk-format-r8g8b8a8-unorm
			       image-handle
			       vk-image-layout-undefined
			       vk-image-layout-transfer-dst-optimal)
      (copy-buffer-to-image device
			    command-pool
			    graphics-queue
			    image-buffer
			    image-handle
			    width
			    height)
      (transition-image-layout device
			       command-pool
			       graphics-queue
			       vk-format-r8g8b8a8-unorm
			       image-handle
			       vk-image-layout-transfer-dst-optimal
			       vk-image-layout-shader-read-only-optimal))))

(define create-texture-image
  (lambda (physical-device device command-pool graphics-queue swapchain texture-path)
    (with-image texture-path
		(lambda (image)
		  (let ((buffer (create-host-buffer physical-device
						    device
						    (make-heap-data (image-data-pointer image)
								    (image-data-size image)))))
		    (create-image-for-texture physical-device
					      device
					      command-pool
					      graphics-queue
					      swapchain
					      (buffer-handle buffer)
					      image))))))


(define texture-image
  (create-texture-image physical-device device command-pool graphics-queue swapchain texture-path))
