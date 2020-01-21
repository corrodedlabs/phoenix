(library (vulkan)

  (export make-vulkan-instance
	  create-debug-utils-messenger
	  get-procedure-address
	  get-physical-devices)

  (import (chezscheme)
	  (ffi)
	  (glfw)

	  (vulkan structure-types))

  (define v (load-shared-object "libvulkan.so.1"))

  (include "vulkan/ftype.scm")
  (include "vulkan/instance.scm")
  (include "vulkan/devices.scm"))
