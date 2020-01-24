



(define get-physical-devices
  (lambda (instance)
    (call-with-array-pointer vk-physical-device
			     (lambda (count devices)
			       (vk-enumerate-physical-devices instance count devices)))))

#!eof

--------------------------------------------
(load "vulkan/devices.scm")

(define devices-arr (get-physical-devices ins))
(define physical-device-ptr (cdr devices-arr))

(find-queue-family physical-device-ptr (window-details-surface window-obj))

