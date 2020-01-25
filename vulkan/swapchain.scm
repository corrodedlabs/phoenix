
(define-record-type swapchain-details
  (fields surface-format
	  present-mode
	  extent))

(define choose-swapchain-settings
  (lambda (surface-formats present-modes surface-capabilities)

    (define choose-surface-format
      (lambda ()
	(or (vk-surface-format-khr-pointer-find
	    (lambda (fmt)
	      (and (equal? (vk-surface-format-khr-format fmt) vk-format-b8g8r8a8-unorm)
		 (equal? (vk-surface-format-khr-color-space fmt)
			 vk-color-space-srgb-nonlinear-khr)))
	    surface-formats)
	   (vk-surface-format-khr-pointer-car surface-formats))))

    (define choose-present-modes
      (lambda ()
	(or (vk-present-mode-khr-pointer-find (lambda (mode)
					       (equal? mode vk-present-mode-mailbox-khr))
					     present-modes)
	   (vk-present-mode-khr-pointer-car present-modes))))

    (define choose-swapchain-extent
      (lambda ()
	;; todo this should ideally be:
	;; max(capabilities.minImageExtent.width,
 	;; min(capabilities.maxImageExtent.width, actualExtent.width));
	;; actualExtent comes from window dimension
	(make-vk-extent-2d (vk-surface-capabilities-max-image-extent-width capabilities)
			   (vk-surface-capabilities-max-image-extent-height capabilities))))
    
    (make-swapchain-details (choose-surface-format)
			    (choose-present-modes)
			    (choose-swapchain-extent))))

(define query-swapchain-details
  (lambda (physical-device surface)

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


    (let ((capabilities (get-capabilities physical-device surface))
	  (surface-formats (get-surface-formats physical-device surface))
	  (present-modes (get-present-modes physical-device surface)))
      (and (swapchain-compatible? surface-formats present-modes)
	 (choose-swapchain-settings surface-formats present-modes capabilities)))))

#!eof

(load "vulkan/swapchain.scm")
(define surface-ptr (window-details-surface window-obj))
(define capabilities (get-capabilities physical-device-ptr
				       (window-details-surface window-obj)))


(define surface-formats (get-surface-formats physical-device-ptr surface-ptr))
(define present-modes (get-present-modes physical-device-ptr surface-ptr))

(ftype-pointer->sexpr (array-pointer-raw-ptr surface-formats))

(swapchain-compatible? surface-formats present-modes)

(choose-swapchain-settings surface-formats present-modes)

(query-swapchain-details physical-device-ptr surface-ptr)
