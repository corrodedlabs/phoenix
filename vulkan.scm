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

  (define-syntax define-vulkan-struct
    (syntax-rules ()
      ((_ struct-name ((member-name . member-type) ...))
       (define-foreign-struct struct-name
	 [(sType . vk-structure-type)
	  (next  . uptr)
	  (member-name . member-type) ...]))))

  (define-vulkan-struct vk-application-info
    ((application-name . uptr)
     (application-version . int)
     (engine-name . uptr)
     (engine-version . int)
     (api-version . int)))

  (define-vulkan-struct vk-instance-create-info
    ((flags . int)
     (application-info . (* vk-application-info))
     (enabled-layer-count . int)
     (enabled-layer-names .  uptr)
     (enabled-extension-count . int)
     (enabled-extension-names . uptr)))


  (define +validation-layer+ "VK_LAYER_KHRONOS_validation")
  (define +validation-extension+ "VK_EXT_debug_utils")


  (define (make-vulkan-instance validation?)
    
    (define create-instance
      (lambda (instance-info)
	(let ((f (foreign-procedure "vkCreateInstance" ((* vk-instance-create-info) uptr uptr) int))
	      (instance (make-foreign-object uptr)))
	  (case (f instance-info 0 (ftype-pointer-address instance))
	    ((0) instance)
	    (else (raise "cannot create vulkan instance "))))))

    ;;todo should strings->ptr just return the address instead of pointer?
    (define get-required-extensions
      (lambda ()
	(let ((glfw-extensions-info (begin (glfw-init)
					   (glfw-get-required-instance-extensions))))
	  (cond
	   (validation?  (let ((extensions (append (ptr->strings glfw-extensions-info)
						   (list +validation-extension+))))
			   (cons (length extensions)
				 (ftype-pointer-address (strings->ptr extensions)))))
	   (else glfw-extensions-info)))))

    (let* ((app-info (make-vk-application-info vk-structure-type-application-info
  					       0
					       "engine"
  					       #x010000
  					       "phoenix"
  					       #x010000
  					       #x010000))
	   (extensions (get-required-extensions))
	   (layers-info (cond
			 (validation? (cons 1
					    (ftype-pointer-address
					     (strings->ptr (list +validation-layer+)))))
			 (else (cons 0 #f))))
	   (instance-info (make-vk-instance-create-info vk-structure-type-instance-create-info
							0
							0
							app-info
							(car layers-info)
							(cdr layers-info)
							(car extensions)
							(cdr extensions))))
      (create-instance instance-info))))

#|

=====================================================================================

(load "vulkan.scm")
(import (ffi))
(import (glfw))
(import (vulkan))

(define validation? #t)

(make-vulkan-instance #f)

(get-required-extensions)

|#
