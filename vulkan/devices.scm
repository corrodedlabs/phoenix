(library (vulkan devices)

  (export )

  (import (chezscheme)
	  (ffi)
	  (vulkan instance))

  (define _enumerate-physical-devices (foreign-procedure "vkEnumeratePhysicalDevices"
							 ((& uptr) (* int) uptr)
							 int)))
