
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
	(lambda ()
	  (begin (f command-buffer)
		 (let ((submit-info (make-vk-submit-info submit-info 0
							 0
							 (null-pointer vk-semaphore)
							 (null-pointer flags)
							 1
							 command-buffer
							 0
							 (null-pointer vk-semaphore))))
		   (vk-end-command-buffer command-buffer)
		   (vk-queue-submit graphics-queue 1 submit-info 0)
		   (vk-queue-wait-idle graphics-queue))))
	(lambda ()
	  ;; (vk-free-command-buffers device command-pool 1 command-buffer)
	  #f
	  )))))


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
       (displayln "creating buffer fir " usage)
       (vk-create-buffer device info 0 buffer)
       buffer))))

(define allocate-memory
  (lambda (physical-device device buffer-ptr mode)

    (define get-memory-requirements
      (lambda (buffer)
	(let ((requirements (make-foreign-object vk-memory-requirements)))
	  (vk-get-buffer-memory-requirements device buffer requirements)
	  requirements)))

    
    (let* ((required-properties (case mode
				  ((host-local)
				   (bitwise-ior vk-memory-property-host-visible-bit
						vk-memory-property-host-coherent-bit))
				  ((gpu-local)
				   vk-memory-property-device-local-bit)))
	   
	   (memory-requirements (get-memory-requirements buffer-ptr))

	   (memory-index
	    (find-memory-type-index physical-device
				    (vk-memory-requirements-memory-type-bits
				     memory-requirements)
				    required-properties))

	   (alloc-info (make-vk-memory-allocate-info
			memory-allocate-info 0
			(vk-memory-requirements-size memory-requirements)
			memory-index))
	   (memory (make-foreign-object vk-device-memory)))
      (displayln "Alloc size" (vk-memory-requirements-size memory-requirements))
      (vk-allocate-memory device alloc-info 0 memory)
      memory)))

;; copies data b/w two gpu buffers
(define copy-buffer-data
  (lambda (device command-pool graphics-queue src-buffer dst-buffer size)
    (displayln "copying buffer data of size " size)
    (let ((copy-region (make-vk-buffer-copy 0 0 size)))
      (execute-command-buffer device
			      command-pool
			      graphics-queue
			      (lambda (command-buffer)
				(displayln "command-buffer is" command-buffer)
				(vk-cmd-copy-buffer command-buffer
						    src-buffer
						    dst-buffer
						    1
						    copy-region))))))


(define data-size
  (lambda (data)
    (cond
     ((vertex-input? (car data)) (sizeof-vertex-input-arr data))
     ((number? (car data)) (* 4 (length data))))))

;; copy data from cpu to gpu host local memory
(define copy-data
  (lambda (device memory data)
    (let ((data-ptr (make-foreign-object uptr))

	  (size (data-size data)))
      (vk-map-memory device memory 0 size 0 (ftype-pointer-address data-ptr))
      (displayln "size if " size)
      (memcpy (ftype-pointer-address data-ptr)
	      (ftype-pointer-address
	       (array-pointer-raw-ptr
		(cond
		 ((vertex-input? (car data))
		  (list->float-pointer-array
		   (map (lambda (value)
			  (let ((ptr (make-foreign-object float)))
			    (ftype-set! float () ptr value)
			    ptr)) 
			(vertices->list data))))
		 ((number? (car data)) (list->u32-pointer-array
					(map (lambda (value)
					       (displayln "setting value" value)
					       (let ((ptr (make-foreign-object u32)))
						 (ftype-set! u32 () ptr value)
						 ptr)) 
					     data))))))
	      size)
      (vk-unmap-memory device memory))))


;; buffer created using HOST_VISIBLE and HOST_COHERENT
;; can be used as staging buffer
(define create-host-buffer
  (lambda (physical-device device data)

    (let* ((size (data-size data))
	   (buffer-ptr (create-new-buffer device size host-local))
	   (memory (allocate-memory physical-device device buffer-ptr host-local)))
      (vk-bind-buffer-memory device buffer-ptr memory 0)
      (copy-data device memory data)
      (displayln "ok creating a host local buffer for data" data)
      (make-buffer buffer-ptr memory size))))

;; create high performance gpu buffer
;; a staging buffer will be used to copy the data over
;; different usage may be made available using identifier syntax
(define create-gpu-local-buffer
  (lambda (physical-device device graphics-queue data usage)
    (let* ((staging-buffer (create-host-buffer physical-device device data))
	   (size (data-size data))
	   (gpu-buffer-ptr (create-new-buffer device size gpu-local usage))
	   (memory (allocate-memory physical-device device gpu-buffer-ptr gpu-local)))
      (displayln "fuck thi " size)
      (vk-bind-buffer-memory device gpu-buffer-ptr memory 0)
      (copy-buffer-data device
      			command-pool
      			graphics-queue
      			(buffer-handle staging-buffer)
      			gpu-buffer-ptr
      			size)
      (displayln "ok creating a gpu local buffer for data" data)
      (make-buffer gpu-buffer-ptr memory size))))


;; Sample usage

(define size (fold-left + 0 (map vertex-input-total-size vertices)))
(define buffer-ptr (create-new-buffer device size host-local))

;; (define memory (allocate-memory buffer-ptr))

;; (define memory-requirements (get-memory-requirements buffer))

(define physical-device (vulkan-state-physical-device vs))
;; (define buf (create-host-buffer physical-device device vertices))

(define graphics-queue (car (vulkan-state-queues vs)))
(define vertex-buffer (create-gpu-local-buffer physical-device
					       device
					       graphics-queue
					       vertices
					       vk-buffer-usage-vertex-buffer-bit))

(define index-buffer (create-gpu-local-buffer physical-device
					      device
					      graphics-queue
					      indices
					      vk-buffer-usage-index-buffer-bit))

;; (define memory (buffer-memory buf))
;; (define data-size (buffer-size buf))



;; Vertex buffers

;; (define create-vertex-buffer
;;   (lambda (device command-pool graphics-queue)
;;     ))

#!eof

(begin (load "vk.scm")
       (load "vulkan/pipeline.scm")
       (load "vulkan/buffers.scm"))
