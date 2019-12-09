(library (vulkan)

  (import (chezscheme)
	  (ffi))

  (define vulkan (load-shared-object "libvulkan.so"))


  (define-enum-ftype vk-structure-type
    vk-structure-type-application-info)

  (define-ftype cstring (* char))

  (define-foreign-struct vk-application-info
    [(sType . vk-structure-type)
     (next . uptr)
     (application-name . cstring)
     (application-version . int)
     (engine-name . cstring)
     (engine-version . int)
     (api-version . int)])

  (define app-info (make-vk-application-info vk-structure-type-application-info
					     0
					     "engine"
					     #x010000
					     "phoenix"
					     #x010000
					     #x010000))

  (vk-application-info-application-name app-info))
