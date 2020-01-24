

(define get-capabilities
  (lambda (physical-device surface)
    (let ((capabilites (make-foreign-object vk-surface-capabilities)))
      (vk-get-physical-device-surface-capabilities-khr physical-device
						       surface
						       capabilites)
      capabilites)))

(define get-surface-formats
  (lambda (physical-device surface)
    (call-with-array-pointer vk-surface-format-khr
			     (lambda (count formats)
			       (vk-get-physical-device-surface-formats-khr physical-device
									   surface
									   count
									   formats)))))

#!eof

(load "vulkan/swapchain.scm")
(define surface-ptr (window-details-surface window-obj))
(define capabilities (get-capabilities physical-device-ptr
				       (window-details-surface window-obj)))


(get-surface-formats physical-device-ptr surface-ptr)
