

(trace-define-syntax define-vulkan-struct
  (syntax-rules ()
    ((_ struct-name ((member-name . member-type) ...))
     (define-foreign-struct struct-name
       [(sType . vk-structure-type)
	(next  . uptr)
	(member-name . member-type) ...]))))

(define-ftype uint32-t unsigned-32)
(define-ftype flags uint32-t)
(define-ftype vk-bool32 unsigned-32)

(define-syntax callback
  (syntax-rules ()
    ((_ f (<args> ...) <ret>)
     (let ([code (foreign-callable f (<args> ...) <ret>)])
       (lock-object code)
       (foreign-callable-entry-point code)))))


(define camel-case->kebab-case
  (lambda (str)
    (list->string
     (let lp ((str (reverse (string->list str)))
	      (i 0)
	      (dest '()))
       (cond
	((null? str) (apply append dest)) 
	
	((and (char-upper-case? (car str))
	    (< 1 (length str))
	    (not (char-upper-case? (cadr str)))) (lp (cdr str)
						   (1+ i)
						   (cons (list #\- (char-downcase (car str)))
							 dest)))

	((char-upper-case? (car str)) (lp (cdr str)
					  (1+ i)
					  (cons (list (char-downcase (car str))) dest)))
	
	(else (lp (cdr str)
		  (1+ i)
		  (cons (list (car str)) dest))))))))


(trace-define-syntax define-vulkan-command
  (lambda (stx)
    (syntax-case stx ()
      [(_ command (argument-types ...))
       (let ((command-string (symbol->string (syntax->datum #'command))))
	 (with-syntax ((command-name (datum->syntax #'command
						    command-string))
		       (_ffi-proc (datum->syntax #'command
						 (string->symbol
						  (string-append "_" command-string))))
		       (ffi-proc (datum->syntax #'command
						(string->symbol
						 (camel-case->kebab-case command-string))))
		       ((arg-names ...) (map (lambda (t) (datum->syntax #'command (gensym)))
					   #'(argument-types ...))))
	   #'(begin (define _ffi-proc
		      (foreign-procedure command-name (argument-types ...) int))

		    (define ffi-proc
		      (lambda (arg-names ...)
			(case (_ffi-proc arg-names ...)
			  ((0) #t)
			  (else (error "vulkan command failed" command-name))))))))])))


;;;;;;;;;;;;;;;;;;;;;;;
;; Instance Creation ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define-vulkan-struct vk-application-info
  ((application-name . uptr)
   (application-version . uint32-t)
   (engine-name . uptr)
   (engine-version . uint32-t)
   (api-version . uint32-t)))

(define-vulkan-struct vk-instance-create-info
  ((instance-create-flags . flags)
   (application-info . (* vk-application-info))
   (enabled-layer-count . unsigned-32)
   (enabled-layer-names . uptr)
   (enabled-extension-count . unsigned-32)
   (enabled-extension-names . uptr)))


(define-enum-ftype vk-debug-utils-message-severity
  (severity-verbose #x00000001)
  (severity-info #x00000010)
  (severity-warning #x00000100)
  (severity-error #x00001000))

(define-enum-ftype vk-debug-utils-message-type
  (general-message #x00000001)
  (validation-message #x00000002)
  (performance-message #x00000004))

;; todo fix ftype-set! to account for array
;; extra case needs to be added to ffi/construct-make-def to account for array type
(define-vulkan-struct vk-debug-utils-label
  ((label-name . uptr)
   (color . uptr)))

(define-enum-ftype object-type
  unknown
  instance
  physical-device
  device
  queue
  semaphore
  command-buffer
  fence
  device-memory
  buffer
  image
  event
  query-pool
  buffer-view
  image-view
  shader-module
  pipeline-cache
  pipeline-layout
  render-pass
  pipeline
  descriptor-set-layout
  sampler
  descriptor-pool
  descriptor-set
  framebuffer
  command-pool
  (sampler-ycbcr-conversion 1000156000)
  (descriptor-update-template 1000085000)
  (surface-khr  1000000000)
  (swapchain-khr  1000001000)
  (display-khr  1000002000)
  (display-mode-khr  1000002001)
  (debug-report-callback-ext  1000011000)
  (object-table-nvx  1000086000)
  (indirect-commands-layout-nvx  1000086001)
  (debug-utils-messenger-ext  1000128000)
  (validation-cache-ext  1000160000)
  (acceleration-structure-nv  1000165000)
  (performance-configuration-intel  1000210000)
  (descriptor-update-template-khr  1000085000)
  (sampler-ycbcr-conversion-khr  1000156000))


(define-vulkan-struct vk-debug-utils-object-name-info
  ((object-type . object-type)
   (object-handle . int)
   (object-name . uptr)))

(define-vulkan-struct vk-debug-utils-messenger-callback-data
  ((flags . int)
   (message-id-name . uptr)
   (message-id-number . int)
   (message . uptr)
   (queue-label-count . int)
   (queue-labels . (* vk-debug-utils-label))
   (cmd-buffer-label-count . int)
   (cmd-buffer-labels . (* vk-debug-utils-label))
   (object-count . int)
   (objects . (* vk-debug-utils-object-name-info))))

(define-ftype vk-callback (function (vk-debug-utils-message-severity
				     vk-debug-utils-message-type
				     (* vk-debug-utils-messenger-callback-data)
				     uptr)
				    unsigned-32))

(define-vulkan-struct vk-debug-utils-messenger-create-info
  ((flags . int)
   (message-severity . vk-debug-utils-message-severity)
   (message-type . vk-debug-utils-message-type)
   (user-callback . (* vk-callback))
   (user-data . uptr)))



(define-ftype vk-instance uptr)

;;;;;;;;;;;;;
;; Surface ;;
;;;;;;;;;;;;;

(define-ftype vk-surface uptr)

;; assumes libglfw is loaded
(define-vulkan-command glfwCreateWindowSurface ((& vk-instance) uptr uptr (* vk-surface)))

;;;;;;;;;;;;;;;;;;;;;;
;; Physical Devices ;;
;;;;;;;;;;;;;;;;;;;;;;

(define-ftype vk-physical-device uptr)

(define-vulkan-command
  vkEnumeratePhysicalDevices
  ((& vk-instance) (* unsigned-32) (* vk-physical-device)))

;;;;;;;;;;;;
;; Queues ;;
;;;;;;;;;;;;

(define-foreign-struct vk-extent-3d
  ((width . unsigned-32)
   (height . unsigned-32)
   (depth . unsigned-32)))

(define-foreign-struct vk-queue-family-properties
  ((queue-flags . unsigned-32)
   (queue-count . unsigned-32)
   (timestamp-valid-bits . unsigned-32)
   (min-image-transfer-granularity . vk-extent-3d)))

(define-enum-ftype vk-queue-flag-bits
  (vk-queue-graphics-bit  #x00000001)
  (vk-queue-compute-bit  #x00000002)
  (vk-queue-transfer-bit  #x00000004)
  (vk-queue-sparse-binding-bit  #x00000008)
  (vk-queue-protected-bit  #x00000010)
  (vk-queue-flag-bits-max-enum  #x7fffffff))

(define vkGetPhysicalDeviceQueueFamilyProperties
  (foreign-procedure "vkGetPhysicalDeviceQueueFamilyProperties"
		     ((& vk-physical-device) (* unsigned-32) (* vk-queue-family-properties))
		     void))

(define-vulkan-command vkGetPhysicalDeviceSurfaceSupportKHR
  ((& vk-physical-device) unsigned-32 (& vk-surface) (* vk-bool32)))

(define-ftype vk-queue uptr)

;;;;;;;;;;;;;;;;;;;;
;; Logical Device ;;
;;;;;;;;;;;;;;;;;;;;

(define-vulkan-struct vk-device-queue-create-info
  ((flags . unsigned-32)
   (queue-family-index . unsigned-32)
   (queue-count . unsigned-32)
   (queue-priorities . (* float))))

(define-vulkan-struct vk-device-create-info
  ((flags . unsigned-32)
   (queue-create-info-count . unsigned-32)
   (queue-create-infos . (* vk-device-queue-create-info))
   (enabled-layer-count . unsigned-32)
   (enabled-layer-names . (* uptr))
   (enabled-extension-count . unsigned-32)
   (enabled-extension-names . (* uptr))
   (enabled-features . uptr)))

(define-ftype vk-device uptr)

(define-vulkan-command vkCreateDevice
  ((& vk-physical-device) (* vk-device-create-info) uptr (* vk-device)))

(define vk-get-device-queue
  (foreign-procedure "vkGetDeviceQueue"
		     ((& vk-device) unsigned-32 unsigned-32 (* vk-queue))
		     void))

;;;;;;;;;;;;;;;
;; swapchain ;;
;;;;;;;;;;;;;;;

(define-foreign-struct vk-extent-2d
  ((width . unsigned-32)
   (height . unsigned-32)))

(define-enum-ftype vk-surface-transform-flag-bits
  (vk-surface-transform-identity-bit-khr  #x00000001)
  (vk-surface-transform-rotate-90-bit-khr  #x00000002)
  (vk-surface-transform-rotate-180-bit-khr  #x00000004)
  (vk-surface-transform-rotate-270-bit-khr  #x00000008)
  (vk-surface-transform-horizontal-mirror-bit-khr  #x00000010)
  (vk-surface-transform-horizontal-mirror-rotate-90-bit-khr  #x00000020)
  (vk-surface-transform-horizontal-mirror-rotate-180-bit-khr  #x00000040)
  (vk-surface-transform-horizontal-mirror-rotate-270-bit-khr  #x00000080)
  (vk-surface-transform-inherit-bit-khr  #x00000100)
  (vk-surface-transform-flag-bits-max-enum-khr  #x7fffffff))

(define-foreign-struct vk-surface-capabilities
  ((min-image-count . unsigned-32)
   (max-image-count . unsigned-32)
   (current-extent . vk-extent-2d)
   (min-image-extent . vk-extent-2d)
   (max-image-extent . vk-extent-2d)
   (max-image-array-layers . unsigned-32)
   (supported-transforms . flags)
   (current-transform . vk-surface-transform-flag-bits)
   (supported-composite-alpha . flags)
   (supported-usage-flags . flags)))

(define-vulkan-command vkGetPhysicalDeviceSurfaceCapabilitiesKHR
  ((& vk-physical-device) (& vk-surface) (* vk-surface-capabilities)))

(define-foreign-struct vk-surface-format-khr
  ((format . vk-format)
   (color-space . vk-color-space-khr)))
(define-collection-lambdas vk-surface-format-khr)


(define-vulkan-command vkGetPhysicalDeviceSurfaceFormatsKHR
  ((& vk-physical-device) (& vk-surface) (* unsigned-32) (* vk-surface-format-khr)))


(define-enum-ftype vk-present-mode-khr
  (vk-present-mode-immediate-khr  0)
  (vk-present-mode-mailbox-khr  1)
  (vk-present-mode-fifo-khr  2)
  (vk-present-mode-fifo-relaxed-khr  3)
  (vk-present-mode-shared-demand-refresh-khr  1000111000)
  (vk-present-mode-shared-continuous-refresh-khr  1000111001)
  (vk-present-mode-max-enum-khr  #x7fffffff))

(define-vulkan-command vkGetPhysicalDeviceSurfacePresentModesKHR
  ((& vk-physical-device) (& vk-surface) (* unsigned-32) (* vk-present-mode-khr)))


#!eof

--------------------------------------------

(import (ffi)
	(vulkan structure-types))
(load "vulkan/enums.scm")
(load "vulkan/ftype.scm")

(define v (load-shared-object "libvulkan.so.1"))

(library-directories '("./thunderchez" "."))
