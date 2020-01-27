(library (vulkan)

  (export setup-vulkan)

  (import (chezscheme)
	  (ffi)
	  (glfw)

	  (vulkan structure-types))

  (define v (load-shared-object "libvulkan.so.1"))

  (include "vulkan/enums.scm")
  (include "vulkan/ftype.scm")
  (include "vulkan/instance.scm")
  (include "vulkan/surface.scm")

  (include "vulkan/queues.scm")
  (include "vulkan/devices.scm")
  (include "vulkan/swapchain.scm")

  (define setup-vulkan
    (lambda ()
      (let* ((instance (init-vulkan))
	     (_ (glfw-init))
	     (window  (setup-window instance 1366 768))
	     (physical-device (array-pointer-raw-ptr (get-physical-devices instance)))
	     (queue-index (find-queue-family physical-device
					     (window-details-surface window)))
	     (device (create-logical-device physical-device queue-index))
	     (queues (create-queue-handles device))
	     )
	(display "all done")
	(newline)))))


;;; (load "vulkan.scm")
