
;; Framebuffers

(define image-views (swapchain-image-views swapchain-details))

;; returns a list of framebuffers created for each of the swapchain image views
(define create-framebuffers
  (lambda (device swapchain pipeline)

    (define (framebuffer-info swapchain-image-view)
      (make-vk-frame-buffer-create-info framebuffer-create-info 0 0
					(pointer-ref-value (pipeline-render-pass pipeline))
					1
					swapchain-image-view
					(vk-extent-2d-width (swapchain-extent swapchain))
					(vk-extent-2d-height (swapchain-extent swapchain))
					1))
    
    (map (lambda (image-view)
	   (let ((info (framebuffer-info image-view))
		 (framebuffer (make-foreign-object vk-frame-buffer)))
	     (vk-create-framebuffer device info 0 framebuffer)
	     framebuffer))
	 (swapchain-image-views swapchain))))

(define image-view (car image-views))
(define framebuffers (create-framebuffers device swapchain-details pipeline))


;; Command pool

(define create-command-pool
  (lambda (device queue-index)
    (let ((info (make-vk-command-pool-create-info command-pool-create-info 0 0
						  queue-index))
	  (command-pool (make-foreign-object vk-command-pool)))
      (vk-create-command-pool device info 0 command-pool)
      command-pool)))

(define queue-index (vulkan-state-queue-index vs))
(define command-pool (create-command-pool device queue-index))

;; Buffers

(define find-memory-type-index
  (lambda (physical-device required-type required-properties)

    (define supported-properties
      (let ((properties (make-foreign-object vk-physical-device-memory-properties)))
	(vk-get-physical-device-memory-properties physical-device properties)
	properties))

    (find (lambda (i)
	    (displayln "checking index:" i)
	    (and (bitwise-and required-type
			    (bitwise-arithmetic-shift-left i 1))
	       (equal? required-properties
		       (bitwise-and (ftype-ref vk-physical-device-memory-properties
					       (memory-types i property-flags)
					       supported-properties)
				    required-properties))))
	  (iota (ftype-ref vk-physical-device-memory-properties
			   (memory-type-count)
			   supported-properties)))))

;; functions to work with gpu buffers

(define-record-type buffer (fields handle memory size))

;; buffer created using HOST_VISIBLE and HOST_COHERENT
;; can be used as staging buffer
(define create-host-buffer
  (lambda (device data)

    (define new-buffer
      (lambda (size)
	(let (( info (make-vk-buffer-create-info buffer-create-info 0 0
						 size
						 vk-buffer-usage-transfer-src-bit
						 vk-sharing-mode-exclusive
						 0
						 (null-pointer unsigned-32)))
	      ( b (make-foreign-object vk-buffer)))
	  (vk-create-buffer device info 0 b)
	  b)))

    (define get-memory-requirements
      (lambda (buffer)
	(let ((requirements (make-foreign-object vk-memory-requirements)))
	  (vk-get-buffer-memory-requirements device buffer requirements)
	  requirements)))

    (define allocate-memory
      (lambda (buffer-ptr)
	(let* ((memory-requirements (get-memory-requirements buffer-ptr))

	       (memory-index
		(find-memory-type-index physical-device
					(vk-memory-requirements-memory-type-bits
					 memory-requirements)
					(bitwise-ior vk-memory-property-host-visible-bit
						     vk-memory-property-host-coherent-bit)))

	       (alloc-info (make-vk-memory-allocate-info
			    memory-allocate-info 0
			    (vk-memory-requirements-size memory-requirements)
			    memory-index))
	       (memory (make-foreign-object vk-device-memory)))
	  (vk-allocate-memory device alloc-info 0 memory)
	  memory)))

    (define copy-bytevector
      (foreign-procedure "memcpy" (uptr u8* size_t) void))

    ;; copy data from cpu to gpu host local memory
    (define copy-data
      (lambda (memory data-size)
	(let ((data (make-foreign-object uptr)))
	  (vk-map-memory device memory 0 data-size 0 (ftype-pointer-address data))
	  (copy-bytevector (ftype-pointer-address data)
			   (cdr (vertices->bytevector vertices)) data-size)
	  (vk-unmap-memory device memory))))

    (let* ((size (fold-left + 0 (map vertex-input-total-size data)))
	   (buffer-ptr (new-buffer size))
	   (memory (allocate-memory buffer-ptr)))
      (vk-bind-buffer-memory device buffer-ptr memory 0)
      (copy-data memory size)
      (make-buffer buffer-ptr memory size))))

(define buf (create-host-buffer device vertices))
;; (define physical-device (vulkan-state-physical-device vs))

(define memory (buffer-memory buf))
(define data-size (buffer-size buf))



;; Vertex buffers

;; (define create-vertex-buffer
;;   (lambda (device command-pool graphics-queue)
;;     ))
