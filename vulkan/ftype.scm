

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


(meta define camel-case->kebab-case
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
(define-vulkan-command glfwCreateWindowSurface ((& vk-instance) uptr uptr uptr))

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
(define-collection-lambdas vk-present-mode-khr)

(define-vulkan-command vkGetPhysicalDeviceSurfacePresentModesKHR
  ((& vk-physical-device) (& vk-surface) (* unsigned-32) (* vk-present-mode-khr)))

(define-enum-ftype vk-composite-alpha-flag-bits-khr
  (vk-composite-alpha-opaque-bit-khr  #x00000001)
  (vk-composite-alpha-pre-multiplied-bit-khr  #x00000002)
  (vk-composite-alpha-post-multiplied-bit-khr  #x00000004)
  (vk-composite-alpha-inherit-bit-khr  #x00000008)
  (vk-composite-alpha-flag-bits-max-enum-khr  #x7fffffff))

(define-enum-ftype vk-image-usage-flag-bits
  (vk-image-usage-transfer-src-bit  #x00000001)
  (vk-image-usage-transfer-dst-bit  #x00000002)
  (vk-image-usage-sampled-bit  #x00000004)
  (vk-image-usage-storage-bit  #x00000008)
  (vk-image-usage-color-attachment-bit  #x00000010)
  (vk-image-usage-depth-stencil-attachment-bit  #x00000020)
  (vk-image-usage-transient-attachment-bit  #x00000040)
  (vk-image-usage-input-attachment-bit  #x00000080)
  (vk-image-usage-shading-rate-image-bit-nv  #x00000100)
  (vk-image-usage-fragment-density-map-bit-ext  #x00000200)
  (vk-image-usage-flag-bits-max-enum  #x7fffffff))

(define-enum-ftype vk-sharing-mode
  vk-sharing-mode-exclusive
  vk-sharing-mode-concurrent)

(define-ftype vk-swapchain uptr)

(define-vulkan-struct vk-swapchain-create-info-khr
  ((flags . unsigned-32)
   (surface .  uptr)
   (min-image-count . unsigned-32)
   (image-format . vk-format)
   (image-color-space . vk-color-space-khr)
   (image-extent . vk-extent-2d)
   (image-array-layers . unsigned-32)
   (image-usage . flags)
   (image-sharing-mode . vk-sharing-mode)
   (queue-family-index-count . unsigned-32)
   (queue-family-indices . (* unsigned-32))
   (pre-transform . vk-surface-transform-flag-bits)
   (composite-alpha . vk-composite-alpha-flag-bits-khr)
   (present-mode . vk-present-mode-khr)
   (clipped . vk-bool32)
   (old-swapchain . vk-swapchain)))

(define-vulkan-command vkCreateSwapchainKHR
  ((& vk-device) (* vk-swapchain-create-info-khr) uptr (* vk-swapchain)))

(define-ftype vk-image uptr)

(define-vulkan-command vkGetSwapchainImagesKHR
  ((& vk-device) (& vk-swapchain) (* unsigned-32) (* vk-image)))

;; swapchain image views

(define-enum-ftype vk-image-view-create-flag-bits
  (vk-image-view-create-fragment-density-map-dynamic-bit-ext #x00000001)
  (vk-image-view-create-flag-bits-max-enum #x7fffffff))

(define-enum-ftype vk-image-view-type
  vk-image-view-type-1d
  vk-image-view-type-2d
  vk-image-view-type-3d
  vk-image-view-type-cube
  vk-image-view-type-1d-array
  vk-image-view-type-2d-array
  vk-image-view-type-cube-array)

(define-enum-ftype vk-component-swizzle
  vk-component-swizzle-identity
  vk-component-swizzle-zero
  vk-component-swizzle-one
  vk-component-swizzle-r
  vk-component-swizzle-g
  vk-component-swizzle-b
  vk-component-swizzle-a)

(define-foreign-struct vk-component-mapping
  ((r . vk-component-swizzle)
   (g . vk-component-swizzle)
   (b . vk-component-swizzle)
   (a . vk-component-swizzle)))

(define-enum-ftype vk-image-aspect-flag-bits
  (vk-image-aspect-color-bit #x00000001)
  (vk-image-aspect-depth-bit  #x00000002)
  (vk-image-aspect-stencil-bit  #x00000004)
  (vk-image-aspect-metadata-bit #x00000008)
  (vk-image-aspect-plane-0-bit  #x00000010)
  (vk-image-aspect-plane-1-bit  #x00000020)
  (vk-image-aspect-plane-2-bit  #x00000040)
  (vk-image-aspect-memory-plane-0-bit-ext  #x00000080)
  (vk-image-aspect-memory-plane-1-bit-ext  #x00000100)
  (vk-image-aspect-memory-plane-2-bit-ext  #x00000200)
  (vk-image-aspect-memory-plane-3-bit-ext  #x00000400))

(define-foreign-struct vk-image-subresource-range
  ((aspect-mask . flags)
   (base-mip-level . unsigned-32)
   (level-count . unsigned-32)
   (base-array-layer . unsigned-32)
   (layer-count . unsigned-32)))

(define-vulkan-struct vk-image-view-create-info
  ((flags . flags)
   (image . vk-image)
   (view-type . vk-image-view-type)
   (format . vk-format)
   (components . vk-component-mapping)
   (sub-resource-range . vk-image-subresource-range)))

(define-ftype vk-image-view uptr)

(define-vulkan-command vkCreateImageView
  ((& vk-device) (* vk-image-view-create-info) uptr (* vk-image-view)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Graphics Pipeline ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define-ftype vk-shader-module uptr)

(define-vulkan-struct vk-shader-module-create-info
  ((flags . flags)
   (code-size . size_t)
   (code . (* unsigned-32))))

(define-vulkan-command vkCreateShaderModule
  ((& vk-device) (* vk-shader-module-create-info) uptr (* vk-shader-module)))

(define-enum-ftype vk-shader-stage-flag-bits
  (vk-shader-stage-vertex-bit  #x00000001)
  (vk-shader-stage-tessellation-control-bit  #x00000002)
  (vk-shader-stage-tessellation-evaluation-bit  #x00000004)
  (vk-shader-stage-geometry-bit  #x00000008)
  (vk-shader-stage-fragment-bit  #x00000010)
  (vk-shader-stage-compute-bit  #x00000020)
  (vk-shader-stage-all-graphics  #x0000001f)
  (vk-shader-stage-all  #x7fffffff)
  (vk-shader-stage-raygen-bit-nv  #x00000100)
  (vk-shader-stage-any-hit-bit-nv  #x00000200)
  (vk-shader-stage-closest-hit-bit-nv  #x00000400)
  (vk-shader-stage-miss-bit-nv  #x00000800)
  (vk-shader-stage-intersection-bit-nv  #x00001000)
  (vk-shader-stage-callable-bit-nv  #x00002000)
  (vk-shader-stage-task-bit-nv  #x00000040)
  (vk-shader-stage-mesh-bit-nv  #x00000080)
  (vk-shader-stage-flag-bits-max-enum  #x7fffffff))

(define-vulkan-struct vk-pipeline-shader-stage-create-info
  ((flags . flags)
   (stage . vk-shader-stage-flag-bits)
   (module . vk-shader-module)
   (name . uptr)
   ;; todo add specialization info when needed
   (specialization-info . uptr)))

#!eof

--------------------------------------------

(import (ffi)
	(vulkan structure-types))
(load "vulkan/enums.scm")
(load "vulkan/ftype.scm")

(define v (load-shared-object "libvulkan.so.1"))

(library-directories '("./thunderchez" "."))
