
(define texture-path "textures/udejafqg_2K_Albedo.jpg")

;; (define swapchain (vulkan-state-swapchain vs))

(define-record-type texture-data (fields image-view sampler))

(define-record-type texture-property
  (fields sampler-mode min-lod max-lod border-color))

(define create-texture-sampler
  (lambda (device texture-prop)
    (match texture-prop
      (($ texture-property sampler-mode min-lod max-lod border-color)
       (let ((info (make-vk-sampler-create-info sampler-create-info
						0
						0
						vk-filter-linear
						vk-filter-linear
						vk-sampler-mipmap-mode-linear
						sampler-mode
						sampler-mode
						sampler-mode
						0.0
						vk-true
						16.0
						vk-false
						vk-compare-op-always
						min-lod
						max-lod
						border-color
						vk-false))
	     (sampler (make-foreign-object vk-sampler)))
	 (vk-create-sampler device info 0 sampler)
	 sampler)))))

(define create-texture-data
  (case-lambda
   ((physical-device device command-pool graphics-queue swapchain)
    (create-texture-data physical-device device command-pool graphics-queue swapchain texture-path))
   ((physical-device device command-pool graphics-queue swapchain texture-path)
    (let ((texture-image (create-texture-image physical-device
					       device
					       command-pool
					       graphics-queue
					       swapchain
					       texture-path))
	  (sampler (create-texture-sampler device
					   (make-texture-property vk-sampler-address-mode-repeat
								  0.0
								  0.0
								  vk-border-color-float-opaque-black))))
      (make-texture-data (gpu-image-view texture-image) sampler)))))
