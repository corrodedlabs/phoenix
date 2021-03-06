;; Synchronization utilities for the gpu

(define *debug* #f)

;; Also contains code to render the command buffers

(define +frames-in-flight+ 8)

;; we will synchronise the gpu flow using semaphores and fences
;; this record captures the synchronization objects
(define-record-type sync-object
  (nongenerative)
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

(define *current-frame* 0)

;; timeout in nanosecs
(define +timeout+ #xFFFFFFFFFFFFFFFF)

(define-record-type frame-state  (nongenerative) (fields current-frame images-in-flight))

(define make-present-info
  (lambda (swapchain signal-semaphore image-index-ptr)
    (let ((result (make-foreign-object int)))
      (make-vk-present-info present-info-khr 0 1 signal-semaphore 1 swapchain image-index-ptr result))))

(define *run-draw-loop* #f)

(define draw-next-frame
  (lambda (window device swapchain queues uniform-buffers cmd-buffers sync-objects state)
    (display "starting draw loop") (newline)
    (let ((image-index (make-foreign-object u32))
	  (uniform-buffers (list->vector uniform-buffers))
	  (cmd-buffers-arr (vk-command-buffer-pointer-map identity cmd-buffers))
	  (swapchain-handle (swapchain-handle swapchain))
	  (graphics-queue (car queues))
	  (present-queue (cdr queues)))
      (let lp ((state state)
	       (eye-position (cons (cdr (vector-ref uniform-buffers 0))
				   (make-vector3 10.0 13.0 1.8)))
	       (i 0))
	(match state
	  (($ frame-state current-frame images-in-flight)
	   (match (list-ref sync-objects current-frame)
	     (($ sync-object
		 available-semaphore finished-semaphore in-flight-fence)		
	      (vk-wait-for-fences device 1 in-flight-fence 1 +timeout+)
	      (vk-acquire-next-image-khr device
					 swapchain-handle
					 +timeout+
					 available-semaphore
					 0
					 image-index)
	      (let ((index (read-unsigned-32 image-index)))
		(cond
		 ((vector-ref images-in-flight index) =>
		  (lambda (image-in-flight)
		    (vk-wait-for-fences device 1 image-in-flight 1 +timeout+))))
		(vector-set! images-in-flight index in-flight-fence)
		(let ((cmd-buffer (list-ref cmd-buffers-arr (read-unsigned-32 image-index)))
		      (wait-dst-mask (make-foreign-object flags)))
		  (ftype-set! flags
			      ()
			      wait-dst-mask
			      vk-pipeline-stage-color-attachment-output-bit)
		  (vk-reset-fences device 1 in-flight-fence)
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
		  (vk-queue-present-khr present-queue
					(make-present-info swapchain-handle
							   finished-semaphore
							   image-index))
		  (poll-events))
		(lp (make-frame-state (mod (fx+ current-frame 1) +frames-in-flight+)
				      images-in-flight)
		    (update-uniform-buffer device
					   (vector-ref uniform-buffers index)
					   (car eye-position)
					   (cdr eye-position)
					   (get-movement-direction window))
		    (+ 1 i)))))))))))


;; (define run (lambda ()
;; 	      (draw-next-frame window device swapchain uniform-buffers cmd-buffers sync-objects initial-state)))

;; (define start-loop
;;   (case-lambda
;;    (() (start-loop #f))
;;    ((threaded?)
;;     (set! *run-draw-loop* #t)
;;     (if threaded?
;; 	(fork-thread run)
;; 	(run)))))


;; (define stop-loop
;;   (lambda ()
;;     (set! *run-draw-loop* #f)))


#!eof


(start-loop)

(stop-loop)
