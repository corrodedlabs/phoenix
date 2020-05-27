

(define get-queue-family-properties
  (lambda (physical-device)
    (with-new-array-pointer vk-queue-family-properties
			    (lambda (count-ptr array-ptr)
			      (vkGetPhysicalDeviceQueueFamilyProperties physical-device
									count-ptr
									array-ptr)))))


(define find-queue-family
  (lambda (physical-device surface)

    (define surface-support?
      (lambda (family-index)
	(let ((supported? (make-foreign-object vk-bool32)))
	  (vk-get-physical-device-surface-support-khr physical-device
						      family-index
						      surface
						      supported?)
	  (= 1 (read-unsigned-32 supported?)))))

    (define supported-queue-family?
      (lambda (family-prop i)
	;; looking for a queues which supports both graphics and presentation
	;; these could be different queues
	;; todo handle different queues
	(and (bitwise-ior (vk-queue-family-properties-queue-flags family-prop)
			vk-queue-graphics-bit
			vk-queue-compute-bit)
	   (surface-support? i))))

    
    (let ((family-props-arr (get-queue-family-properties physical-device)))
      (let lp ((i 0))
	(cond
	 ((= i (array-pointer-length family-props-arr)) #f)

	 ((supported-queue-family? (ftype-&ref vk-queue-family-properties
					       ()
					       (array-pointer-raw-ptr family-props-arr)
					       i)
				   i)
	  i)

	 (else (lp (1+ i))))))))



(define create-queue-handles
  (lambda (device)
    (let ((graphics-queue (make-foreign-object vk-queue))
	  (presentation-queue (make-foreign-object vk-queue)))
      (vk-get-device-queue device 0  0 graphics-queue)
      (vk-get-device-queue device 0  0 presentation-queue)
      (cons graphics-queue presentation-queue))))
