



(define get-physical-devices
  (lambda (instance)
    (call-with-array-pointer vk-physical-device
			     (lambda (count devices)
			       (vk-enumerate-physical-devices instance count devices)))))

#!eof

--------------------------------------------
(load "vulkan/devices.scm")

(define devices-arr (get-physical-devices ins))

(define get-queue-family-properties
  (lambda (physical-device)
    (call-with-array-pointer vk-queue-family-properties
			     (lambda (count-ptr array-ptr)
			       (vkGetPhysicalDeviceQueueFamilyProperties physical-device
									 count-ptr
									 array-ptr)))))

(define physical-device (cdr devices-arr))

(define family-props-arr (get-queue-family-properties physical-device))



(vk-queue-family-properties-queue-count
 (ftype-&ref vk-queue-family-properties () (cdr family-props-arr) 2))

