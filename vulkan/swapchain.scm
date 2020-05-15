
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
	(displayln "present modes" present-modes)
	(or (vk-present-mode-khr-pointer-find (lambda (mode)
					       (or (equal? mode vk-present-mode-mailbox-khr)
						  (equal? mode vk-present-mode-fifo-khr)))
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

;; creates  the swapchain handle
;; returns a cons cell (swapchain-handle . swapchain-details)
(define create-swapchain-handle
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
					     (pointer-ref-value surface)
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
      (vk-create-swapchain-khr device
			       (create-swapchain-info swapchain-details)
			       0
			       swapchain)
      (cons swapchain swapchain-details))))

(define create-swapchain-images
  (lambda (device swapchain)
    (with-new-array-pointer vk-image
			    (lambda (count images)
			      (vk-get-swapchain-images-khr device swapchain count images)))))

(define-collection-lambdas vk-image)

(define create-swapchain-image-views
  (lambda (device swapchain-details images)

    (define create-swapchain-image-view-info
      (lambda (image)
	(let ((components (make-vk-component-mapping vk-component-swizzle-identity
						     vk-component-swizzle-identity
						     vk-component-swizzle-identity
						     vk-component-swizzle-identity))
	      (surface-format (swapchain-details-surface-format swapchain-details))
	      (subresource-range (make-vk-image-subresource-range
				  vk-image-aspect-color-bit
				  0
				  1
				  0
				  1)))
	  (make-vk-image-view-create-info image-view-create-info 0 0
					  (pointer-ref-value image)
					  vk-image-view-type-2d
					  (vk-surface-format-khr-format surface-format)
					  components
					  subresource-range))))

    (vk-image-pointer-map (lambda (image)
			    (let ((info (create-swapchain-image-view-info image))
				  (image-view (make-foreign-object vk-image-view)))
			      (vk-create-image-view device info 0 image-view)
			      image-view))
			  images)))

;; record to store the information of swapchain and its images and views
;; this struct will be available in the vulkan state and can be used at any stage to
;; refer to the contents of swapchain or apply relevant functions
(define-record-type swapchain
  (fields handle images image-views format extent))

(define (create-swapchain physical-device device surface queue-index)
  (let* ((swapchain-info (create-swapchain-handle physical-device
						  device
						  surface
						  queue-index))
	 (swapchain (car swapchain-info))
	 (swapchain-details (cdr swapchain-info))

	 (images (create-swapchain-images device swapchain))
	 (image-views (create-swapchain-image-views device
						    swapchain-details
						    images)))
    (make-swapchain swapchain
		    images
		    image-views
		    (swapchain-details-surface-format swapchain-details)
		    (swapchain-details-extent swapchain-details))))

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
(define vs (setup-vulkan))

(vulkan-state-swapchain vs)

(define swapchain-details-obj (create-swapchain (vulkan-state-physical-device vs)
						(vulkan-state-device vs)
						(vulkan-state-surface vs)
						(vulkan-state-queue-index vs)))

(define swapchain-images
  (create-swapchain-images (vulkan-state-device vs)
			   (swapchain-details-swapchain swapchain-details-obj)))

(array-pointer? swapchain-images)

(array-pointer-length swapchain-images)


(define swapchain-image-views
  (create-swapchain-image-views (vulkan-state-device vs)
				swapchain-details-obj
				swapchain-images))
