
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

;; creates and submits a single command buffer
;; the created command buffer is passed to the function f
;; once f returns the command buffer is submitted to the queue and
;; we wait for device idle
(define execute-command-buffer
  (lambda (device command-pool graphics-queue f)
    (let ((command-buffer (make-foreign-object vk-command-buffer)))
      (dynamic-wind
	(lambda ()
	  (let ((info
		 (make-vk-command-buffer-allocate-info command-buffer-allocate-info
						       0
						       0
						       (pointer-ref-value command-pool)
						       vk-command-buffer-level-primary
						       1
						       0))
		(begin-info (make-vk-command-buffer-begin-info
			     command-buffer-begin-info
			     0
			     vk-command-buffer-usage-one-time-submit-bit
			     (null-pointer vk-command-buffer-inheritance-info))))
	    (vk-allocate-command-buffers device info command-buffer)
	    (vk-begin-command-buffer command-buffer begin-info)))
	(lambda () (f command-buffer))
	(lambda ()
	  (let ((submit-info (make-vk-submit-info submit-info 0
						  0
						  (null-pointer vk-semaphore)
						  (null-pointer flags)
						  1
						  command-buffer
						  0
						  (null-pointer vk-semaphore))))
	    (vk-end-command-buffer command-buffer)
	    (vk-queue-submit graphics-queue 1 submit-info (null-pointer vk-fence))
	    (vk-queue-wait-idle graphics-queue)
	    (vk-free-command-buffers device command-pool 1 command-buffer)))))))


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


;; Modes for creating a new buffer
(define-syntax host-local (identifier-syntax 'host-local))
(define-syntax gpu-local (identifier-syntax 'gpu-local))

;; buffer usage


(define create-new-buffer
  (case-lambda
    ((device buffer-size mode) (create-new-buffer device buffer-size mode 0))
    ((device buffer-size mode usage-flags)
     (let* ((usage (case mode
		     ((host-local) vk-buffer-usage-transfer-src-bit)
		     ((gpu-local) (bitwise-ior vk-buffer-usage-transfer-dst-bit
					       usage-flags))))
	    (info (make-vk-buffer-create-info buffer-create-info 0 0
					      buffer-size
					      usage
					      vk-sharing-mode-exclusive
					      0
					      (null-pointer unsigned-32)))
	    (buffer (make-foreign-object vk-buffer)))
       (vk-create-buffer device info 0 buffer)
       buffer))))


;; buffer created using HOST_VISIBLE and HOST_COHERENT
;; can be used as staging buffer
(define create-host-buffer
  (lambda (physical-device device data)

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

    ;; copy data from cpu to gpu host local memory
    (define copy-data
      (lambda (memory data-size)
	(let ((data (make-foreign-object uptr)))
	  (vk-map-memory device memory 0 data-size 0 (ftype-pointer-address data))
	  (memcpy (ftype-pointer-address data)
		  (ftype-pointer-address
		   (array-pointer-raw-ptr
		    (list->float-pointer-array (map
						(lambda (value)
						  (let ((ptr (make-foreign-object float)))
						    (ftype-set! float () ptr value)
						    ptr))
						(vertices->list vertices)))))
		  data-size)
	  (vk-unmap-memory device memory))))

    (let* ((size (fold-left + 0 (map vertex-input-total-size data)))
	   (buffer-ptr (create-new-buffer device size host-local))
	   (memory (allocate-memory buffer-ptr)))
      (vk-bind-buffer-memory device buffer-ptr memory 0)
      (copy-data memory size)
      (make-buffer buffer-ptr memory size))))

;; high performance gpu buffer
;; to get the data here, use a host local staging buffer
(define create-gpu-local-buffer
  (lambda (device)
    ))


;; Sample usage

(define size (fold-left + 0 (map vertex-input-total-size vertices)))
(define buffer-ptr (create-new-buffer device size host-local))

;; (define memory (allocate-memory buffer-ptr))

;; (define memory-requirements (get-memory-requirements buffer))

(define physical-device (vulkan-state-physical-device vs))
(define buf (create-host-buffer physical-device device vertices))


;; (define memory (buffer-memory buf))
;; (define data-size (buffer-size buf))

;; (define graphics-queue (car (vulkan-state-queues vs)))

;; (execute-command-buffer device
;; 			command-pool
;; 			graphics-queue
;; 			(lambda (command-buffer)
;; 			  (vk-cmd-copy-buffer command-buffer )))

;; Vertex buffers

;; (define create-vertex-buffer
;;   (lambda (device command-pool graphics-queue)
;;     ))
