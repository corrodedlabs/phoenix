;; Synchronization utilities for the gpu

;; Also contains code to render the command buffers

(define +frames-in-flight+ 2)

;; we will synchronise the gpu flow using semaphores and fences
;; this record captures the synchronization objects
(define-record-type sync-object
  (fields image-available-semaphore
	  render-finished-semaphore
	  in-flight-fence))

(define init-sync-objects
  (lambda (device)
    (map (lambda (i)
	   (let ((semaphore-info (make-vk-semaphore-create-info semaphore-create-info 0 0))
		 (fence-info (make-vk-fence-create-info fence-create-info
							0
							vk-fence-create-signaled-bit))
		 (available-semaphore (make-foreign-object vk-semaphore))
		 (finished-semaphore (make-foreign-object vk-semaphore))
		 (fence (make-foreign-object vk-fence)))
	     (vk-create-semaphore device semaphore-info 0 available-semaphore)
	     (vk-create-semaphore device semaphore-info 0 finished-semaphore)
	     (vk-create-fence device fence-info 0 fence)
	     (make-sync-object available-semaphore finished-semaphore fence)))
	 (iota +frames-in-flight+))))

(define sync-objects (init-sync-objects device))

(define *current-frame* 0)

;; timeout in nanosecs
(define +timeout+ #xFFFFFFFFFFFFFFFF)

(define-record-type frame-state (fields current-frame images-in-flight))

(define make-present-info
  (lambda (swapchain signal-semaphore image-index-ptr)
    (let ((result (make-foreign-object int)))
      (displayln "swaochain for present info" swapchain)
      (make-vk-present-info present-info-khr 0 1 signal-semaphore 1 swapchain image-index-ptr result))))


(define draw-next-frame
  (lambda (device swapchain cmd-buffers sync-objects state)
    (let ((image-index (make-foreign-object u32))
	  (cmd-buffers-arr (vk-command-buffer-pointer-map identity cmd-buffers))
	  (swapchain-handle (swapchain-handle swapchain)))
      (let lp ((state state)
	       (i 0))
	;; (if (> i 10) (error "done"))
	(match state
	  (($ frame-state current-frame images-in-flight)
	   (match (list-ref sync-objects current-frame)
	     (($ sync-object
		 available-semaphore finished-semaphore in-flight-fence)
	      (displayln "current frame is " current-frame)
	      (displayln "images in flight" images-in-flight)
	      (displayln "waiting for fence" in-flight-fence)
	      (vk-wait-for-fences device 1 in-flight-fence 1 +timeout+)
	      (displayln "acquiring next image" available-semaphore)
	      (vk-acquire-next-image-khr device
					 swapchain-handle
					 +timeout+
					 available-semaphore
					 0
					 image-index)
	      (displayln "acquired image index" (read-unsigned-32 image-index))
	      (cond
	       ((vector-ref images-in-flight (read-unsigned-32 image-index)) =>
		(lambda (image-in-flight)
		  (displayln "waiting for fence" image-in-flight)
		  (vk-wait-for-fences device 1 image-in-flight 1 +timeout+))))
	      (vector-set! images-in-flight (read-unsigned-32 image-index) in-flight-fence)
	      (let ((cmd-buffer (list-ref cmd-buffers-arr (read-unsigned-32 image-index)))
		    (wait-dst-mask (make-foreign-object flags)))
		(ftype-set! flags () wait-dst-mask vk-pipeline-stage-color-attachment-output-bit)
		(displayln "resetting fences" in-flight-fence)
		(vk-reset-fences device 1 in-flight-fence)
		(displayln "submitting to graphics queue with cmd-buffer " cmd-buffer )
		(vk-queue-submit graphics-queue
				 1
				 (make-vk-submit-info submit-info
						      0
						      1
						      available-semaphore
						      (array-pointer-raw-ptr
						       (list->u32-pointer-array
							(list wait-dst-mask)))
						      1
						      cmd-buffer
						      1
						      finished-semaphore)
				 (pointer-ref-value in-flight-fence))
		(display "submitting to presentation queue") (newline)
		(vk-queue-present-khr present-queue
				      (make-present-info swapchain-handle
							 finished-semaphore
							 image-index)))
	      (display "requesting next frame") (newline)
	      (lp (make-frame-state (mod (fx+ current-frame 1) +frames-in-flight+)
				    images-in-flight)
		  (+ 1 i))))))))))


(draw-next-frame device
		 (vulkan-state-swapchain vs)
		 cmd-buffers
		 sync-objects
		 (make-frame-state 1
				   (list->vector
				    (map (lambda (_) #f) (iota (array-pointer-length cmd-buffers))))))
