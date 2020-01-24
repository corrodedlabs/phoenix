

(define get-queue-family-properties
  (lambda (physical-device)
    (call-with-array-pointer vk-queue-family-properties
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
			vk-queue-graphics-bit)
	   (surface-support? i))))

    
    (let ((family-props-arr (get-queue-family-properties physical-device)))
      (let lp ((i 0))
	(cond
	 ((= i (car family-props-arr)) #f)

	 ((supported-queue-family? (ftype-&ref vk-queue-family-properties
					       ()
					       (cdr family-props-arr)
					       i)
				   i)
	  i)

	 (else (lp (1+ i))))))))
