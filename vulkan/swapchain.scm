

(define get-capabilities
  (lambda (physical-device surface)
    (let ((capabilites (make-foreign-object vk-surface-capabilities)))
      (vk-get-physical-device-surface-capabilities-khr physical-device
						       surface
						       capabilites)
      capabilites)))

(define get-surface-formats
  (lambda (physical-device surface)
    (with-new-array-pointer vk-surface-format-khr
			    (lambda (count formats)
			      (vk-get-physical-device-surface-formats-khr physical-device
									  surface
									  count
									  formats)))))
(define get-present-modes
  (lambda (physical-device surface)
    (with-new-array-pointer vk-present-mode-khr
			    (lambda (count present-modes)
			      (vk-get-physical-device-surface-present-modes-khr
			       physical-device surface count present-modes)))))

(define swapchain-compatible?
  (lambda (surface-formats present-modes)
    (not (or (array-pointer-empty? surface-formats) (array-pointer-empty? present-modes)))))



(define choose-swapchain-settings
  (lambda (surface-formats present-modes)

    (define choose-swapchan-surface-format
      (lambda (surface-formats)
	(or (vk-surface-format-khr-pointer-find
	    (lambda (fmt)
	      (and (equal? (vk-surface-format-khr-format fmt) vk-format-b8g8r8a8-unorm)
		 (equal? (vk-surface-format-khr-color-space fmt)
			 vk-color-space-srgb-nonlinear-khr)))
	    surface-formats)
	   (vk-surface-format-khr-pointer-car surface-formats))))
    
    (choose-swapchan-surface-format surface-formats)))

#!eof

(load "vulkan/swapchain.scm")
(define surface-ptr (window-details-surface window-obj))
(define capabilities (get-capabilities physical-device-ptr
				       (window-details-surface window-obj)))


(define surface-formats (get-surface-formats physical-device-ptr surface-ptr))

(ftype-pointer->sexpr (array-pointer-raw-ptr surface-formats))



(swapchain-compatible? surface-formats
		       (get-present-modes physical-device-ptr surface-ptr))
