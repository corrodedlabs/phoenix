
(define-record-type swapchain-details
  (fields surface-format
	  present-mode
	  extent
	  capabilities))

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
	(make-vk-extent-2d (vk-surface-capabilities-max-image-extent-width
			    surface-capabilities)
			   (vk-surface-capabilities-max-image-extent-height
			    surface-capabilities))))
    
    (make-swapchain-details (choose-surface-format)
			    (choose-present-modes)
			    (choose-swapchain-extent)
			    surface-capabilities)))

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

(define create-swapchain
  (lambda (physical-device device surface queue-index)

    (define create-swapchain-info
      (lambda (swapchain-details)
	(let ((surface-format (swapchain-details-surface-format swapchain-details))
	      (capabilities (swapchain-details-capabilities swapchain-details))
	      (queue-indices-ptr (make-foreign-array unsigned-32 2)))
	  (ftype-set! unsigned-32 () queue-indices-ptr queue-index 0)
	  (ftype-set! unsigned-32 () queue-indices-ptr queue-index 1)
	  (make-vk-swapchain-create-info-khr swapchain-create-info-khr
					     0
					     0
					     (foreign-ref 'uptr
							  (ftype-pointer-address surface)
							  0)
					     (vk-surface-capabilities-max-image-count
					      capabilities)
					     (vk-surface-format-khr-format surface-format)
					     (vk-surface-format-khr-color-space surface-format)
					     (swapchain-details-extent swapchain-details)
					     1
					     vk-image-usage-color-attachment-bit
					     vk-sharing-mode-exclusive
					     2
					     queue-indices-ptr
					     ;; current transform for no transform
					     (vk-surface-capabilities-current-transform
					      capabilities)
					     vk-composite-alpha-opaque-bit-khr
					     (read-int
					      (swapchain-details-present-mode swapchain-details))
					     1 ;; enable clipping
					     0))))

    (let ((swapchain (make-foreign-object vk-swapchain))
	  (swapchain-details (query-swapchain-details physical-device surface)))
      (vk-create-swapchain-khr device (create-swapchain-info swapchain-details) 0 swapchain)
      swapchain)))

(define create-swapchain-image-views
  (lambda (device swapchain)
    (with-new-array-pointer vk-image
			    (lambda (count images)
			      (vk-get-swapchain-images-khr device swapchain count images)))))

#!eof

(load "vulkan/swapchain.scm")
(define surface-ptr (window-details-surface window-obj))

(define capabilities (get-capabilities physical-device-ptr surface-ptr))


(define surface-formats (get-surface-formats physical-device-ptr surface-ptr))
(define present-modes (get-present-modes physical-device-ptr surface-ptr))

(ftype-pointer->sexpr (array-pointer-raw-ptr surface-formats))

(swapchain-compatible? surface-formats present-modes)

(choose-swapchain-settings surface-formats present-modes)



(vk-surface-capabilities-min-image-count capabilities)


(create-swapchain physical-device-ptr device-ptr surface-ptr queue-index)



(vk-surface-capabilities-current-transform (swapchain-details-capabilities swapchain-details))
;; fix height and width settings
(vk-extent-2d-height (swapchain-details-extent swapchain-details))


--- from vulkan state

(define swapchain-images
  (create-swapchain-image-views (vulkan-state-device vs) (vulkan-state-swapchain vs)))


(array-pointer? swapchain-images)

(array-pointer-length swapchain-images)
