(define validation? #t)
(define *validation-extension* "VK_EXT_debug_utils")


(define (VK_MAKE_VERSION major minor patch)
  (logor (ash major 22) (ash minor 12) (ash patch 0)))

(define VK_API_VERSION_1_1 (VK_MAKE_VERSION 1 1 0))

(define get-procedure-address
  (lambda (instance procedure-name)
    ((foreign-procedure "vkGetInstanceProcAddr"
			((& vk-instance) string) uptr) instance procedure-name)))

  ;;;;;;;;;;;;;;;;;;;
;; Vulkan Layers ;;
  ;;;;;;;;;;;;;;;;;;;


(define *validation-layer* "VK_LAYER_KHRONOS_validation")

;; Structure definition
;;
;;   typedef struct VkLayerProperties {
;;     char        layerName[VK_MAX_EXTENSION_NAME_SIZE];
;;     uint32_t    specVersion;
;;     uint32_t    implementationVersion;
;;     char        description[VK_MAX_DESCRIPTION_SIZE];
;; } VkLayerProperties;

(define-foreign-struct layer-properties ((layer-name . uptr)))

(define nullptr (make-ftype-pointer layer-properties 0))

(define _enumerate-layers (foreign-procedure "vkEnumerateInstanceLayerProperties"
					     ((* unsigned-32)  uptr) int))

(define get-supported-layers
  (lambda ()
    (define enumerate-layers
      (lambda (count props)
	(let [(result (_enumerate-layers count props))]
	  (write (format "result is ~s" result))
	  (case result
	    ((0) result)
	    (else (error "failed: " result))))))

    (let [(count (make-foreign-object unsigned-32))]
      (enumerate-layers count 0)
      (write (format "count is ~d \n" (ftype-ref unsigned-32 () count)))
      (let [(layer-props (unbox (malloc (* (read-unsigned-32 count)
					   (ftype-sizeof layer-properties)))))]
	(enumerate-layers count layer-props)
	layer-props))))

;; (get-supported-layers)


  ;;;;;;;;;;;;;;;;;;;;;;;
;; Instance Creation ;;
  ;;;;;;;;;;;;;;;;;;;;;;;

(define (make-vulkan-instance validation?)
  
  (define create-instance
    (lambda (instance-info)
      (let ((_create-instance (foreign-procedure "vkCreateInstance"
						 ((* vk-instance-create-info)
						  uptr
						  (* vk-instance)) int))
	    (instance (make-foreign-object vk-instance)))
	(write "going to create instance now")
	(let ((res (_create-instance instance-info 0 instance)))
	  (case res 
	    ((0) instance)
	    (else (write res) (raise "cannot create vulkan instance ")))))))

  ;;todo should strings->ptr just return the address instead of pointer?
  (define get-required-extensions
    (lambda ()
      (let ((glfw-extensions-info ((lambda ()
				     (glfw-init)
				     (glfw-get-required-instance-extensions)))))
	(cond
	 (validation? (let ((extensions (append (ptr->strings glfw-extensions-info)
						(list *validation-extension*))))
			(cons (length extensions) (strings->ptr extensions))))
	 (else glfw-extensions-info)))))

  
  (let* ((app-info (make-vk-application-info application-info
					     0
					     "engine"
					     VK_API_VERSION_1_1
					     "phoenix"
					     VK_API_VERSION_1_1
					     VK_API_VERSION_1_1))
	 (extensions (get-required-extensions))
	 (layers-info (cond
		       (validation?
			(cons 1 (strings->ptr (list *validation-layer*))))
		       (else (cons 0 #f))))
	 (instance-info (make-vk-instance-create-info instance-create-info
						      0
						      0
						      app-info
						      ;; 0 #f
						      (car layers-info)
						      (ftype-pointer-address (cdr layers-info))
						      ;; 0 #f
						      (car extensions)
						      (ftype-pointer-address (cdr extensions)))))
    (write (ptr->strings layers-info))
    (write (format "layers info cdr is \n"))
    (create-instance instance-info)))

(define debug-callback
  (lambda (message-severity message-type callback-data user-data)
    (write "callback")
    (write message-type)))

(define create-debug-utils-messenger
  (lambda (instance)

    (define-ftype _create-debug-utils-messenger
      (function ( (& vk-instance) (* vk-debug-utils-messenger-create-info) uptr uptr) int))
    
    (define create-debug-utils-info
      (lambda ()
	(let ((cb (make-ftype-pointer vk-callback
				      (callback debug-callback
						(vk-debug-utils-message-severity
						 vk-debug-utils-message-type
						 (* vk-debug-utils-messenger-callback-data)
						 uptr)
						unsigned-32))))
	  (make-vk-debug-utils-messenger-create-info debug-utils-messenger-create-info-ext
						     0
						     0
						     (logor severity-verbose
							    severity-warning
							    severity-error)
						     (logor general-message
							    validation-message
							    performance-message)
						     cb
						     0))))
    
    (let* ((messenger (make-foreign-object uptr))
	   (proc-address (get-procedure-address instance "vkCreateDebugUtilsMessengerEXT"))
	   (_create-instance (ftype-ref _create-debug-utils-messenger
					()
					(make-ftype-pointer _create-debug-utils-messenger
							    proc-address))))
      (_create-instance instance
			(create-debug-utils-info)
			0
			(ftype-pointer-address messenger))
      messenger)))


(define init-vulkan
  (lambda ()
    (let ((instance (make-vulkan-instance #t)))
      (create-debug-utils-messenger instance)
      instance)))


