

(define +device-extensions+ (list "VK_KHR_swapchain"))

(define get-physical-devices
  (lambda (instance)
    (with-new-array-pointer vk-physical-device
			    (lambda (count devices)
			      (vk-enumerate-physical-devices instance count devices)))))

(trace-define find-memory-type-index
	      (lambda (physical-device required-type required-properties)
		(let ((properties (make-foreign-object vk-physical-device-memory-properties)))
		  (vk-get-physical-device-memory-properties physical-device properties)      
		  (find (lambda (i)
			  (and (bitwise-and required-type
					  (bitwise-arithmetic-shift-left i 1))
			     (equal? required-properties
				     (bitwise-and (ftype-ref vk-physical-device-memory-properties
							     (memory-types i property-flags)
							     properties)
						  required-properties))))
			(iota (ftype-ref vk-physical-device-memory-properties
					 (memory-type-count)
					 properties))))))


(define create-logical-device
  (lambda (physical-device queue-index)

    (define create-device-info
      (lambda ()
	(let ((queue-priority (make-foreign-object float)))
	  (ftype-set! float () queue-priority 1.0)
	  (make-vk-device-create-info  device-create-info
				       0
				       0
				       1
				       (make-vk-device-queue-create-info device-queue-create-info
									 0
									 0
									 queue-index
									 1
									 queue-priority)
				       (car *validation-layers-info*)
				       (cdr *validation-layers-info*)
				       1
				       (strings->ptr +device-extensions+)
				       (ftype-pointer-address enabled-features)))))
    
    (let ((device-info (create-device-info))
	  (device (make-foreign-object vk-device)))
      (vk-create-device physical-device device-info 0 device)
      device)))

#!eof

--------------------------------------------
(load "vulkan/queues.scm")
(load "vulkan/devices.scm")


(define devices-arr (get-physical-devices ins))
(define physical-device-ptr (array-pointer-raw-ptr devices-arr))

(define queue-index (find-queue-family physical-device-ptr
				       (window-details-surface window-obj)))



(define device-ptr (create-logical-device physical-device-ptr queue-index))

;; todo make a record
(define queues (create-queue-handles device-ptr))
