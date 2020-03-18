
;; Framebuffers

;; returns a list of framebuffers created for each of the swapchain image views
(define create-framebuffers
  (lambda (physical-device device command-pool graphics-queue swapchain pipeline)

    (define (framebuffer-info . image-views)
      (let ((views (list->vk-image-view-pointer-array image-views)))
	(displayln "views" views)
	(make-vk-frame-buffer-create-info framebuffer-create-info 0 0
					  (pointer-ref-value (pipeline-render-pass pipeline))
					  (array-pointer-length views)
					  (array-pointer-raw-ptr views)
					  (vk-extent-2d-width (swapchain-extent swapchain))
					  (vk-extent-2d-height (swapchain-extent swapchain))
					  1)))
    
    (let ((depth-image-view (create-depth-buffer-image physical-device
						       device
						       command-pool
						       graphics-queue
						       swapchain)))
      (map (lambda (image-view)
	     (let ((info (framebuffer-info image-view (gpu-image-view depth-image-view)))
		   (framebuffer (make-foreign-object vk-frame-buffer)))
	       (vk-create-framebuffer device info 0 framebuffer)
	       framebuffer))
	   (swapchain-image-views swapchain)))))


;; Command pool

(define create-command-pool
  (lambda (device queue-index)
    (let ((info (make-vk-command-pool-create-info command-pool-create-info 0 0
						  queue-index))
	  (command-pool (make-foreign-object vk-command-pool)))
      (vk-create-command-pool device info 0 command-pool)
      command-pool)))


(define allocate-command-buffers
  (case-lambda
    ((device command-pool) (allocate-command-buffers device command-pool 1))
    ((device command-pool num-buffer)
     (let ((info (make-vk-command-buffer-allocate-info command-buffer-allocate-info
						       0
 						       (pointer-ref-value command-pool)
						       vk-command-buffer-level-primary
						       num-buffer))
	   (command-buffer (if (fx=? num-buffer 1)
			     (make-foreign-object vk-command-buffer)
			     (make-foreign-array vk-command-buffer num-buffer))))
       (vk-allocate-command-buffers device info command-buffer)
       command-buffer))))

(define start-command-buffer-recording
  (case-lambda
    ((command-buffer) (start-command-buffer-recording command-buffer #f))
    ((command-buffer one-time-submit?)
     (let ((begin-info (make-vk-command-buffer-begin-info
			command-buffer-begin-info
			0
			(if one-time-submit?
			    vk-command-buffer-usage-one-time-submit-bit
			    0)
			(null-pointer vk-command-buffer-inheritance-info))))
       (vk-begin-command-buffer command-buffer begin-info)
       command-buffer))))

(define end-command-buffer-recording
  (case-lambda
   ((command-buffer)
    (vk-end-command-buffer command-buffer))
   ((command-buffer graphics-queue)
    (vk-end-command-buffer command-buffer)
    (let ((submit-info (make-vk-submit-info submit-info 0
					    0
					    (null-pointer vk-semaphore)
					    (null-pointer flags)
					    1
					    command-buffer
					    0
					    (null-pointer vk-semaphore))))
      (vk-queue-submit graphics-queue 1 submit-info 0)
      (vk-queue-wait-idle graphics-queue)))))

;; creates and submits a single command buffer
;; the created command buffer is passed to the function f
;; once f returns the command buffer is submitted to the queue and
;; we wait for device idle
(define execute-command-buffer
  (lambda (device command-pool graphics-queue f)
    (let ((command-buffer #f))
      (dynamic-wind
	(lambda ()
	  (set! command-buffer
		(start-command-buffer-recording (allocate-command-buffers device command-pool)
						#t)))
	(lambda ()
	  (begin (f command-buffer)
		 (end-command-buffer-recording command-buffer graphics-queue)))
	(lambda ()
	  ;; (vk-free-command-buffers device command-pool 1 command-buffer)
	  #f
	  )))))

;; allocate 'num-buffers' command-buffers and record the command as passed in f
;; to all of them
(define record-command-buffers
  (lambda (device command-pool framebuffers descriptor-sets f)
    (let* ((num-buffers (length framebuffers))
	   (command-buffers (allocate-command-buffers device command-pool num-buffers))
	   (cmd-buffers-ptr (make-array-pointer num-buffers
						command-buffers
						'vk-command-buffer)))
      
      (map (lambda (cmd-buffer frame-buffer descriptor-set)
	     (start-command-buffer-recording cmd-buffer)
	     (f cmd-buffer frame-buffer descriptor-set)
	     (end-command-buffer-recording cmd-buffer))
	   (vk-command-buffer-pointer-map identity cmd-buffers-ptr)
	   framebuffers
	   descriptor-sets)
      cmd-buffers-ptr)))


(define create-command-buffers
  (lambda (device swapchain command-pool pipeline framebuffers descriptor-sets)

    (define clear-values-ptr
      (lambda ()
	(let ((clear-values clear-values)
	      (depth-clear (cons 1.0 0)))
	  (list->vk-clear-value-pointer-array (list (make-vk-clear-value clear-values)
						    (make-vk-clear-value depth-clear))))))

    (define perform-render-pass
      (lambda (cmd-buffer framebuffer descriptor-set vertex-buffer index-buffer render-area clear-values)
	(let ((info (make-vk-render-pass-begin-info (pipeline-render-pass pipeline)
						    framebuffer
						    render-area
						    clear-values))
	      (vertex-buffers (array-pointer-raw-ptr (list->vk-buffer-pointer-array
						      (list (buffer-handle vertex-buffer)))))
	      (offsets (array-pointer-raw-ptr
			(list->u64-pointer-array (map (lambda (v)
							(let ((ptr (make-foreign-object u64)))
							  (ftype-set! u64 () ptr v)
							  ptr))
						      (list 0))))))
	  (vk-cmd-begin-render-pass cmd-buffer
				    info
				    vk-subpass-contents-inline)
	  (vk-cmd-bind-pipeline cmd-buffer
				vk-pipeline-bind-point-graphics
				(pipeline-handle pipeline))
	  (vk-cmd-bind-vertex-buffers cmd-buffer
	  			      0
	  			      1
				      vertex-buffers
	  			      offsets)
	  (vk-cmd-bind-index-buffer cmd-buffer
	  			    (buffer-handle index-buffer)
	  			    0
	  			    vk-index-type-uint32)
	  (vk-cmd-bind-descriptor-sets cmd-buffer
	  			       vk-pipeline-bind-point-graphics
	  			       (pipeline-layout pipeline)
	  			       0
	  			       1
	  			       descriptor-set
	  			       0
	  			       (null-pointer u32))
	  (vk-cmd-draw-indexed cmd-buffer (length indices) 1 0 0 0)
	  (vk-cmd-end-render-pass cmd-buffer)
	  cmd-buffer)))

    (let ((clear-values (clear-values-ptr))
	  (render-area
	   (make-render-area (cons 0 0)
			     (cons (vk-extent-2d-width (swapchain-extent swapchain))
				   (vk-extent-2d-height (swapchain-extent swapchain))))))
      (record-command-buffers device
			      command-pool
			      framebuffers
			      descriptor-sets
			      (lambda (cmd-buffer framebuffer descriptor-set)
				(perform-render-pass cmd-buffer
						     framebuffer
						     descriptor-set
						     vertex-buffer
						     index-buffer
						     render-area
						     clear-values))))))

;; Buffers

;; functions to work with gpu buffers

(define-record-type buffer (fields handle memory size))


;; Modes for creating a new buffer
(define-syntax host-local (identifier-syntax 'host-local))
(define-syntax gpu-local (identifier-syntax 'gpu-local))

;; buffer usage


(define create-new-buffer
  (case-lambda
    ((device buffer-size mode) (create-new-buffer device buffer-size mode #f))
    ((device buffer-size mode usage-flags)
     (let* ((usage (case mode
		     ((host-local) (if usage-flags
				       (bitwise-ior vk-buffer-usage-transfer-src-bit
						    usage-flags)
				       vk-buffer-usage-transfer-src-bit))
		     ((gpu-local) (if usage-flags
				      (bitwise-ior vk-buffer-usage-transfer-dst-bit
						   usage-flags)
				      vk-buffer-usage-transfer-dst-bit))))
	    (info (make-vk-buffer-create-info buffer-create-info 0 0
					      buffer-size
					      usage
					      vk-sharing-mode-exclusive
					      0
					      (null-pointer unsigned-32)))
	    (buffer (make-foreign-object vk-buffer)))
       (displayln "usage flags:" usage-flags)
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


;; record to represent a chunk of heap data which can be copied over to gpu blocks
;; using vkMapMemory and memcpy
(define-record-type heap-data (fields pointer size))


;; copy data from cpu to gpu host local memory
(define copy-data
  (lambda (device memory data)
    (match data
      (($ heap-data data-ptr size)
       (let ((dst-ptr (make-foreign-object uptr)))
	 (vk-map-memory device memory 0 size 0 (ftype-pointer-address dst-ptr))
	 (memcpy (pointer-ref-value dst-ptr) data-ptr size)
	 (vk-unmap-memory device memory))))))

;; the predicates in the cond of sizeof-data and scheme-data->c-pointer
;; represent the types supported

;; get the sizeof data where data consists of scheme values (list? or vertex-input?)
(define sizeof-scheme-data
  (lambda (data)
    (cond
     ((heap-data? data) (heap-data-size data))
     ((vertex-input? (car data)) (sizeof-vertex-input data))
     ((number? (car data)) (* 4 (length data))))))

;; convert scheme values to c pointers representing those values
(define scheme-data->c-pointer
  (lambda (data)
    (ftype-pointer-address
     (array-pointer-raw-ptr
      (cond
       ((vertex-input? (car data))
	(list->float-pointer-array
	 (map-indexed (lambda (value i)
			(displayln "setting value" value)
			(let ((ptr (make-foreign-object float)))
			  (ftype-set! float () ptr value)
			  ptr)) 
		      (let lp ((v (vector->list (vertex-input->vector data)))
			       (flat-coll (list)))
			(cond
			 ((null? v) flat-coll)
			 (else (lp (cdr v)
				   (append flat-coll
					   (apply append
						  (vector->list
						   (vector-map (lambda (_ v) (vector->list v))
							       (car v))))))))))))

       ((inexact? (car data))
	(list->float-pointer-array
	 (map-indexed (lambda (value i)
			(let ((ptr (make-foreign-object float)))
			  (ftype-set! float () ptr value)
			  ptr))
		      data)))

       ((number? (car data))
	(list->u32-pointer-array
	 (map-indexed (lambda (value i)
			(displayln "setting value" value)
			(let ((ptr (make-foreign-object u32)))
			  (ftype-set! u32 () ptr value)
			  ptr)) 
		      data))))))))

(define copy-data-from-scheme
  (lambda (device memory data)
    (let ((data-ptr (make-foreign-object uptr))
	  (size (sizeof-scheme-data data)))
      (copy-data device memory (cond
				((heap-data? data) data)
				(else (make-heap-data (scheme-data->c-pointer data) size)))))))


;; buffer created using HOST_VISIBLE and HOST_COHERENT
;; can be used as staging buffer
(define create-host-buffer
  (case-lambda
   ((physical-device device data) (create-host-buffer physical-device device data #f))
   ((physical-device device data usage)
    (let* ((size (sizeof-scheme-data data))
	   (buffer-ptr (create-new-buffer device size host-local usage))
	   (memory (allocate-memory physical-device device buffer-ptr host-local)))
      (vk-bind-buffer-memory device buffer-ptr memory 0)
      (copy-data-from-scheme device memory data)
      (displayln "ok creating a host local buffer for data" data)
      (make-buffer buffer-ptr memory size)))))

;; create high performance gpu buffer
;; a staging buffer will be used to copy the data over
;; different usage may be made available using identifier syntax
(define create-gpu-local-buffer
  (lambda (physical-device device graphics-queue data usage)
    (let* ((staging-buffer (create-host-buffer physical-device device data))
	   (size (sizeof-scheme-data data))
	   (gpu-buffer-ptr (create-new-buffer device size gpu-local usage))
	   (memory (allocate-memory physical-device device gpu-buffer-ptr gpu-local)))
      (vk-bind-buffer-memory device gpu-buffer-ptr memory 0)
      (copy-buffer-data device
      			command-pool
      			graphics-queue
      			(buffer-handle staging-buffer)
      			gpu-buffer-ptr
      			size)
      (displayln "creating a gpu local buffer for data" data)
      (make-buffer gpu-buffer-ptr memory size))))


;; Uniform buffers

(define-record-type uniform-buffer-data (fields model view projection))

(define (uniform-buffer-data->list data) (mvp-matrix->list data))

(define (extent->uniform-buffer-data extent)
  (let ((width (vk-extent-2d-width extent))
	(height (vk-extent-2d-height extent)))
    (calculate-mvp-matrix width height)))



(define create-uniform-buffers
  (lambda (physical-device device data num-buffers)
    (map (lambda (i)
	   (create-host-buffer physical-device
			       device
			       data
			       vk-buffer-usage-uniform-buffer-bit))
	 (iota num-buffers))))


;; Descriptor sets and pools

;; used to pass on data like uniform buffers and textures to the shaders

(define create-descriptor-pool
  (lambda (device descriptor-count)
    (let* ((uniform-buffer-pool-size (make-vk-descriptor-pool-size vk-descriptor-type-uniform-buffer
								   descriptor-count))
	   (image-sampler-pool-size
	    (make-vk-descriptor-pool-size vk-descriptor-type-combined-image-sampler
					  descriptor-count))
	   (pool-size-ptr (list->vk-descriptor-pool-size-pointer-array
			   (list uniform-buffer-pool-size image-sampler-pool-size)))
	   (info (make-vk-descriptor-pool-create-info descriptor-pool-create-info 0 0
						      descriptor-count
						      (array-pointer-length pool-size-ptr)
						      (array-pointer-raw-ptr pool-size-ptr)))
	   (pool (make-foreign-object vk-descriptor-pool)))
      (vk-create-descriptor-pool device info 0 pool)
      pool)))


(define create-descriptor-sets
  (lambda (device descriptor-pool descriptor-layout uniform-buffers texture-data)

    (define num-sets (length uniform-buffers))

    ;; uniform buffer  object suze
    (define ubo-size (sizeof-scheme-data uniform-buffer-data-list))

    (define allocate-descriptor-sets
      (lambda ()
	(let* ((layouts (array-pointer-raw-ptr
			 (list->vk-descriptor-set-layout-pointer-array
			  (map (lambda (i) descriptor-layout) (iota num-sets)))))
	       (alloc-info (make-vk-descriptor-set-allocate-info
			    descriptor-set-allocate-info
			    0
			    (pointer-ref-value descriptor-pool)
			    num-sets
			    layouts))
	       (set (make-foreign-array vk-descriptor-set num-sets)))
	  (vk-allocate-descriptor-sets device alloc-info set)
	  (make-array-pointer num-sets set 'vk-descriptor-set))))

    (let ((descriptor-sets  (allocate-descriptor-sets)))
      (displayln "ubo size is" ubo-size)
      (map (lambda (uniform-buffer descriptor-set)
	     (let* ((buffer-info
		     (make-vk-descriptor-buffer-info (pointer-ref-value uniform-buffer)
						     0
						     ubo-size))
		    (image-info
		     (make-vk-descriptor-image-info
		      (pointer-ref-value (texture-data-sampler texture-data))
		      (pointer-ref-value (texture-data-image-view texture-data))
		      vk-image-layout-shader-read-only-optimal))
		    (uniform-buffer-write
		     (make-vk-write-descriptor-set write-descriptor-set
						   0
						   (pointer-ref-value descriptor-set)
						   0
						   0
						   1
						   vk-descriptor-type-uniform-buffer
						   (null-pointer vk-descriptor-image-info)
						   buffer-info
						   (null-pointer vk-buffer-view)))
		    (combined-image-sampler-write
		     (make-vk-write-descriptor-set write-descriptor-set
						   0
						   (pointer-ref-value descriptor-set)
						   1
						   0
						   1
						   vk-descriptor-type-combined-image-sampler
						   image-info
						   (null-pointer vk-descriptor-buffer-info)
						   (null-pointer vk-buffer-view)))
		    (write
		     (list->vk-write-descriptor-set-pointer-array
		      (list uniform-buffer-write combined-image-sampler-write))))
	       (vk-update-descriptor-sets device
					  (array-pointer-length write)
					  (array-pointer-raw-ptr write)
					  0
					  0)
	       descriptor-set))
	   (map buffer-handle uniform-buffers)
	   (vk-descriptor-set-pointer-map (lambda (x) x) descriptor-sets)))))


;; Depth buffering



;; Sample usage

(define image-views (swapchain-image-views swapchain-details))
(define device (vulkan-state-device vs))
(define queue-index (vulkan-state-queue-index vs))
(define command-pool (create-command-pool device queue-index))

;; (define size (fold-left + 0 (map vertex-input-total-size vertices)))
;; (define buffer-ptr (create-new-buffer device size host-local))

;; (define memory (allocate-memory buffer-ptr))

;; (define memory-requirements (get-memory-requirements buffer))

(define physical-device (vulkan-state-physical-device vs))
;; (define buf (create-host-buffer physical-device device vertices))

(define graphics-queue (car (vulkan-state-queues vs)))
(define present-queue (cdr (vulkan-state-queues vs)))

(display "creating buffers") (newline)

(define vertex-buffer (create-gpu-local-buffer physical-device
					       device
					       graphics-queue
					       vertices
					       vk-buffer-usage-vertex-buffer-bit))

(displayln "vertex buffer done" vertex-buffer)

(define index-buffer (create-gpu-local-buffer physical-device
					      device
					      graphics-queue
					      indices
					      vk-buffer-usage-index-buffer-bit))

(displayln "index buffer created" index-buffer)

(define image-view (car image-views))

(define framebuffers (create-framebuffers physical-device device command-pool
					  graphics-queue swapchain-details pipeline ))
(displayln "framebuffers created " framebuffers)

(define swapchain (vulkan-state-swapchain vs))
(define extent (swapchain-extent (vulkan-state-swapchain vs)))

(define uniform-buffer-data-list
  (uniform-buffer-data->list (extent->uniform-buffer-data extent)))

(define uniform-buffers
  (create-uniform-buffers physical-device
			  device
			  uniform-buffer-data-list
			  (length framebuffers)))

(define descriptor-count (length uniform-buffers))

(define descriptor-pool (create-descriptor-pool device
						(length uniform-buffers)))

(define descriptor-layout (pipeline-descriptor-set-layout pipeline))
(define num-sets (length uniform-buffers))

(define texture-data (create-texture-data physical-device device command-pool graphics-queue
					  swapchain))

(define sets (create-descriptor-sets device
				     descriptor-pool
				     descriptor-layout
				     uniform-buffers
				     texture-data))

;; (define depth-buffer-image
;;   (create-depth-buffer-image physical-device device command-pool graphics-queue swapchain))

(define clear-values (list 0.0 0.0 0.0 1.0))

(display "going to created cmd buffers") (newline)

(define cmd-buffers
  (create-command-buffers device swapchain command-pool pipeline framebuffers sets))

(displayln "command buffers created" cmd-buffers)

;; (define memory (buffer-memory buf))
;; (define data-size(buffer-size buf))



;; Vertex buffers

;; (define create-vertex-buffer
;;   (lambda (device command-pool graphics-queue)
;;     ))

#!eof

(begin (load "vk.scm")
       (load "vulkan/pipeline.scm")
       (load "vulkan/images.scm")
       (load "vulkan/buffers.scm"))
