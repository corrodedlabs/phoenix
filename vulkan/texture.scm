
(define texture-path "textures/winter.jpeg")

(define texture-image
  (create-texture-image physical-device device command-pool graphics-queue swapchain texture-path))


(define create-texture-sampler
  (lambda (device)
    (let ((info (make-vk-sampler-create-info sampler-create-info
					     0
					     0
					     vk-filter-linear
					     vk-filter-linear
					     vk-sampler-mipmap-mode-linear
					     vk-sampler-address-mode-repeat
					     vk-sampler-address-mode-repeat
					     vk-sampler-address-mode-repeat
					     0.0
					     vk-true
					     16.0
					     vk-false
					     vk-compare-op-always
					     0.0
					     0.0
					     vk-border-color-int-opaque-black
					     vk-false))
	  (sampler (make-foreign-object vk-sampler)))
      (vk-create-sampler device info 0 sampler)
      sampler)))


(define sampler (create-texture-sampler device))
