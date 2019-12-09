(library (vulkan)

  (export make-vulkan-instance)

  (import (chezscheme)
	  (ffi)
	  (glfw))

  (define vulkan (load-shared-object "libvulkan.so"))

  (define-enum-ftype vk-structure-type
    vk-structure-type-application-info
    vk-structure-type-instance-create-info)

  (define-ftype cstring (* char))

  (trace-define-syntax define-vulkan-struct
    (syntax-rules ()
      ((_ struct-name ((member-name . member-type) ...))
       (define-foreign-struct struct-name
	 [(sType . vk-structure-type)
	  (next  . uptr)
	  (member-name . member-type) ...]))))

  (define-vulkan-struct vk-application-info
    ((application-name . cstring)
     (application-version . int)
     (engine-name . cstring)
     (engine-version . int)
     (api-version . int)))

  (define-vulkan-struct vk-instance-create-info
    ((flags . int)
     (application-info . (* vk-application-info))
     (enabled-layer-count . int)
     (enabled-layer-names .  (* char**))
     (enabled-extension-count . int)
     (enabled-extension-names . (* char**))))

  (define (make-vulkan-instance validation?)
    
    (define create-instance
      (lambda (instance-info)
	(let ((f (foreign-procedure "vkCreateInstance" ((* vk-instance-create-info) uptr uptr) int))
	      (instance (make-foreign-object uptr)))
	  (case (f instance-info 0 (ftype-pointer-address instance))
	    ((0) instance)
	    (else (raise "cannot create vulkan instance "))))))

    (let* ((app-info (make-vk-application-info vk-structure-type-application-info
  					       0
  					       "engine"
  					       #x010000
  					       "phoenix"
  					       #x010000
  					       #x010000))
	   (glfw-extensions-info (begin (glfw-init)
					(glfw-get-required-instance-extensions)))
	   (instance-info (make-vk-instance-create-info vk-structure-type-instance-create-info
							0
							0
							app-info
							0
							#f
							(car glfw-extensions-info)
							(cdr glfw-extensions-info))))
      (create-instance instance-info))))
