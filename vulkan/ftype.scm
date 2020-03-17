

(trace-define-syntax define-vulkan-struct
  (syntax-rules ()
    ((_ struct-name ((member-name . member-type) ...))
     (define-foreign-struct struct-name
       [(sType . vk-structure-type)
	(next  . uptr)
	(member-name . member-type) ...]))))

(define-ftype uint32-t unsigned-32)
(define-ftype u32 unsigned-32)
(define-ftype u64 unsigned-64)
(define-ftype flags uint32-t)

(define-ftype i32 integer-32)
(define-ftype i64 integer-64)

(define-collection-lambdas u32)
(define-collection-lambdas u64)
(define-collection-lambdas float)

(define-ftype vk-bool32 unsigned-32)
(define vk-true 1)
(define vk-false 0)

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
			(let ((res (_ffi-proc arg-names ...)))
			  (case res
			    ((0) #t)
			    (else (error "vulkan command failed" command-name res)))))))))])))


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

(define-syntax define-features
  (lambda (stx)
    (syntax-case stx ()
      ((_ id feature ...)
       (with-syntax ((init-pointer (construct-name #'id "init-feature-pointer"))
		     (enable-features (construct-name #'id "enable-features")))
	 #'(begin (define-ftype id
		    (struct (feature vk-bool32) ...))
		  
		  (define init-pointer
		    (lambda ()
		      (let ((ptr (make-foreign-object id)))
			(ftype-set! id (feature) ptr vk-false)
			...
			ptr)))

		  (meta trace-define-syntax enable-features
			(syntax-rules ()
			  ((_ enabled-feature (... ...))
			   (let ((features (init-feature-pointer)))
			     ;; enable required feature
			     (ftype-set! vk-physical-device-features
					 (enabled-feature)
					 features
					 vk-true)
			     (... ...)
			     features))))))))))

(define-features vk-physical-device-features
  robust-buffer-access
  full-draw-index-uint32
  image-cube-array
  independent-blend
  geometry-shader
  tessellation-shader
  sample-rate-shading
  dual-src-blend
  logic-op
  multi-draw-indirect
  draw-indirect-first-instance
  depth-clamp
  depth-bias-clamp
  fill-mode-non-solid
  depth-bounds
  wide-lines
  large-points
  alpha-to-one
  multi-viewport
  sampler-anisotropy
  texture-compression-etc2
  texture-compression-astc-ldr
  texture-compression-bc
  occlusion-query-precise
  pipeline-statistics-query
  vertex-pipeline-stores-and-atomics
  fragment-stores-and-atomics
  shader-tessellation-and-geometry-point-size
  shader-image-gather-extended
  shader-storage-image-extended-formats
  shader-storage-image-multisample
  shader-storage-image-read-without-format
  shader-storage-image-write-without-format
  shader-uniform-buffer-array-dynamic-indexing
  shader-sampled-image-array-dynamic-indexing
  shader-storage-buffer-array-dynamic-indexing
  shader-storage-image-array-dynamic-indexing
  shader-clip-distance
  shader-cull-distance
  shader-float64
  shader-int64
  shader-int16
  shader-resource-residency
  shader-resource-min-lod
  sparse-binding
  sparse-residency-buffer
  sparse-residency-image2-d
  sparse-residency-image3-d
  sparse-residency2-samples
  sparse-residency4-samples
  sparse-residency8-samples
  sparse-residency16-samples
  sparse-residency-aliased
  variable-multisample-rate
  inheritedQueries)

(define enabled-features (enable-features sampler-anisotropy))

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


;; images
(define-ftype vk-image uptr)

(define-enum-ftype vk-image-type
  (vk-image-type-1d  0)
  (vk-image-type-2d  1)
  (vk-image-type-3d  2))

(define-enum-ftype vk-image-tiling
  (vk-image-tiling-optimal  0)
  (vk-image-tiling-linear  1)
  (vk-image-tiling-drm-format-modifier-ext  1000158000))

(define-vulkan-struct vk-image-create-info
  ((flags . flags)
   (image-type . vk-image-type)
   (format . vk-format)
   (extent . vk-extent-3d)
   (mip-levels . u32)
   (array-layers . u32)
   (samples . vk-sample-count-flag-bits)
   (tiling . vk-image-tiling)
   (usage . flags)
   (sharing-mode . vk-sharing-mode)
   (queue-family-index-count . u32)
   (queue-family-indices . (* u32))
   (initial-layout . vk-image-layout)))


;; image barriers

(define-vulkan-struct vk-image-memory-barrier
  ((src-access-mask . flags)
   (dst-access-mask . flags)
   (old-layout . vk-image-layout)
   (new-layout . vk-image-layout)
   (src-queue-family-index . u32)
   (dst-queue-family-index . u32)
   (image . vk-image)
   (subresource-range . vk-image-subresource-range)))


(define-vulkan-command vkCreateImage
  ((& vk-device) (* vk-image-create-info) uptr (* vk-image)))

;; image view

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

;; shader stages

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

(define-collection-lambdas vk-pipeline-shader-stage-create-info)

;; vertex input

(define-enum-ftype vk-vertex-input-rate
  vk-vertex-input-rate-vertex
  vk-vertex-input-rate-instance)


(define-foreign-struct vk-vertex-input-binding-description
  ((binding . unsigned-32)
   (stride . unsigned-32)
   (input-rate . vk-vertex-input-rate)))

(define-foreign-struct vk-vertex-input-attribute-description
  ((location . unsigned-32)
   (binding . unsigned-32)
   (format . vk-format)
   (offset . unsigned-32)))

(define-collection-lambdas vk-vertex-input-attribute-description)

(define-vulkan-struct vk-pipeline-vertex-input-state-create-info
  ((flags . flags)
   (vertex-bindings-description-count . unsigned-32)
   (vertex-bindings-descriptions . (* vk-vertex-input-binding-description))
   (vertex-attribute-description-count . unsigned-32)
   (vertex-attribute-descriptions . (* vk-vertex-input-attribute-description))))

;; pipeline input assembly state

(define-enum-ftype vk-primitive-topology
  vk-primitive-topology-point-list
  vk-primitive-topology-line-list
  vk-primitive-topology-line-strip
  vk-primitive-topology-triangle-list
  vk-primitive-topology-triangle-strip
  vk-primitive-topology-triangle-fan
  vk-primitive-topology-line-list-with-adjacency
  vk-primitive-topology-line-strip-with-adjacency
  vk-primitive-topology-triangle-list-with-adjacency
  vk-primitive-topology-triangle-strip-with-adjacency
  vk-primitive-topology-patch-list)


(define-vulkan-struct vk-pipeline-input-assembly-state-create-info
  ((flags . flags)
   (topology . vk-primitive-topology)
   (primitive-restart-enabled . unsigned-32)))

;; tessellation

(define-vulkan-struct vk-pipeline-tessellation-state-create-info
  ((flags . flags)
   (patch-control-points . unsigned-32)))

;; viewports

(define-foreign-struct vk-viewport
  ((x . float)
   (y . float)
   (width . float)
   (height . float)
   (min-depth . float)
   (max-depth . float)))

(define-foreign-struct vk-offset-2d  ((x . unsigned-32) (y . unsigned-32)))
(define-foreign-struct vk-rect-2d ((offset . vk-offset-2d) (extent . vk-extent-2d)))

(define-foreign-struct vk-offset-3d ((x . i32) (y . i32) (z . i32)))

(define-vulkan-struct vk-pipeline-viewport-state-create-info
  ((flags . flags)
   (viewport-count . unsigned-32)
   (viewports . (* vk-viewport))
   (scissor-count . unsigned-32)
   (scissors . (* vk-rect-2d))))

;; rasterizer

(define-enum-ftype vk-polygon-mode
  vk-polygon-mode-fill
  vk-polygon-mode-line
  vk-polygon-mode-point
  (vk-polygon-mode-fill-rectangle-nv 1000153000)
  ;; todo some values are missing here
  )

(define-enum-ftype vk-cull-mode-flag-bits
  vk-cull-mode-none
  (vk-cull-mode-front-bit #x00000001)
  (vk-cull-mode-back-bit #x00000002)
  (vk-cull-mode-front-and-back #x00000003))

(define-enum-ftype vk-front-face
  vk-front-face-counter-clockwise
  vk-front-face-clockwise
  ;;todo more here
  )

(define-vulkan-struct vk-pipeline-rasterization-state-create-info
  ((flags . flags)
   (depth-clamp-enable . unsigned-32)
   (rasterizer-discard-enable . unsigned-32)
   (polygon-mode . vk-polygon-mode)
   (cull-mode . flags)
   (front-face . vk-front-face)
   (depth-bias-enable . unsigned-32)
   (depth-bias-constant-factor . float)
   (depth-bias-clamp . float)
   (depth-bias-slope-factor . float)
   (line-width . float)))

;; multisampling

(define-vulkan-struct vk-pipeline-multisample-state-create-info
  ((flags . flags)
   (rasterization-samples . vk-sample-count-flag-bits)
   (sample-shading-enable . unsigned-32)
   (min-sample-shading . float)
   (sample-mask . (* unsigned-32))
   (alpha-to-coverage-enable . unsigned-32)
   (alpha-to-one-enable . unsigned-32)))

;; depth stencil

(define-foreign-struct vk-stencil-op-state
  ((fail-op . vk-stencil-op)
   (pass-op . vk-stencil-op)
   (depth-fail-op . vk-stencil-op)
   (compare-op . vk-compare-op)
   (write-mask . unsigned-32)
   (reference . unsigned-32)))

(define-vulkan-struct vk-pipeline-depth-stencil-state-create-info
  ((flags . flags)
   (depth-test-enable . vk-bool32)
   (depth-write-enable . vk-bool32)
   (depth-compare-op . vk-compare-op)
   (depth-bounds-test-enable . vk-bool32)
   (stencil-test-enable . vk-bool32)
   (front . vk-stencil-op-state)
   (back . vk-stencil-op-state)
   (min-depth-bounds . float)
   (max-depth-bounds . float)))

;; color blending

;; vk-blend-factor, vk-logic-op & vk-blend-op defined in enums.scm

(define-enum-ftype vk-color-component-flag-bits
  (vk-color-component-r-bit #x00000001)
  (vk-color-component-g-bit #x00000002)
  (vk-color-component-b-bit #x00000004)
  (vk-color-component-a-bit #x00000008))


(define-foreign-struct vk-pipeline-color-blend-attachment-state
  ((enable-blend . unsigned-32)
   (src-color-blend-factor . vk-blend-factor)
   (dest-color-blend-factor . vk-blend-factor)
   (color-blend-op . vk-blend-op)
   (src-alpha-blend-factor . vk-blend-factor)
   (dst-alpha-blend-factor . vk-blend-factor)
   (alpha-blend-op . vk-blend-op)
   (color-write-mask . flags)))


(define-vulkan-struct vk-pipeline-color-blend-state-create-info
  ((flags . flags)
   (enable-logic-op . unsigned-32)
   (logic-op . vk-logic-op)
   (attachment-count . unsigned-32)
   (attachments . (* vk-pipeline-color-blend-attachment-state))
   (blend-constants . (array 4 float))))


;; pipeline layout and vk-descriptor-sets

(define-ftype vk-descriptor-set-layout uptr)

(define-collection-lambdas vk-descriptor-set-layout)

(define-enum-ftype vk-descriptor-type
  vk-descriptor-type-sampler
  vk-descriptor-type-combined-image-sampler
  vk-descriptor-type-sampled-image
  vk-descriptor-type-storage-image
  vk-descriptor-type-uniform-texel-buffer
  vk-descriptor-type-storage-texel-buffer
  vk-descriptor-type-uniform-buffer
  vk-descriptor-type-storage-buffer
  vk-descriptor-type-uniform-buffer-dynamic
  vk-descriptor-type-storage-buffer-dynamic
  vk-descriptor-type-input-attachment
  (vk-descriptor-type-inline-uniform-block-ext  1000138000)
  (vk-descriptor-type-acceleration-structure-nv  1000165000))

(define-ftype vk-sampler uptr)

;; todo add more types for Vk-Sampler

(define-foreign-struct vk-descriptor-set-layout-binding
  ((binding . unsigned-32)
   (descriptor-type . vk-descriptor-type)
   (descriptor-count . unsigned-32)
   (stage-flags . flags)
   (immutable-samplers . (* vk-sampler))))

(define-collection-lambdas vk-descriptor-set-layout-binding)

(define-vulkan-struct vk-descriptor-set-layout-create-info
  ((flags . flags)
   (binding-count . unsigned-32)
   (bindings . (* vk-descriptor-set-layout-binding))))

(define-vulkan-command vkCreateDescriptorSetLayout
  ((& vk-device) (* vk-descriptor-set-layout-create-info) uptr (* vk-descriptor-set-layout)))

(define-foreign-struct vk-push-constant-range
  ((stage-flags . flags)
   (offset . unsigned-32)
   (size . unsigned-32)))

;; pipeline layout

(define-vulkan-struct vk-pipeline-layout-create-info
  ((flags . flags)
   (set-layout-count . unsigned-32)
   (set-layouts . (* vk-descriptor-set-layout))
   (push-constant-range-count . unsigned-32)
   (push-constant-ranges . (* vk-push-constant-range))))

(define-ftype vk-pipeline-layout uptr)

(define-vulkan-command vkCreatePipelineLayout
  ((& vk-device) (* vk-pipeline-layout-create-info) uptr (* vk-pipeline-layout)))

(define-ftype vk-pipeline uptr)

;; dynamic state

(define-enum-ftype vk-dynamic-state
  (vk-dynamic-state-viewport  0)
  (vk-dynamic-state-scissor  1)
  (vk-dynamic-state-line-width  2)
  (vk-dynamic-state-depth-bias  3)
  (vk-dynamic-state-blend-constants  4)
  (vk-dynamic-state-depth-bounds  5)
  (vk-dynamic-state-stencil-compare-mask  6)
  (vk-dynamic-state-stencil-write-mask  7)
  (vk-dynamic-state-stencil-reference  8)
  (vk-dynamic-state-viewport-w-scaling-nv  1000087000)
  (vk-dynamic-state-discard-rectangle-ext  1000099000)
  (vk-dynamic-state-sample-locations-ext  1000143000)
  (vk-dynamic-state-viewport-shading-rate-palette-nv  1000164004)
  (vk-dynamic-state-viewport-coarse-sample-order-nv  1000164006)
  (vk-dynamic-state-exclusive-scissor-nv  1000205001)
  (vk-dynamic-state-line-stipple-ext  1000259000))

(define-vulkan-struct vk-pipeline-dynamic-state-create-info
  ((flags . flags)
   (dynamic-state-count . unsigned-32)
   (dynamic-states . (* vk-dynamic-state))))

;; render pass

(define-ftype vk-render-pass uptr)

(define-enum-ftype vk-attachment-load-op
  vk-attachment-load-op-load
  vk-attachment-load-op-clear
  vk-attachment-load-dont-care)

(define-enum-ftype vk-attachment-store-op
  vk-attachment-store-op-store
  vk-attachment-store-dont-care)

(define-foreign-struct vk-attachment-description
  ((flags . flags)
   (format . vk-format)
   (samples . vk-sample-count-flag-bits)
   (load-op . vk-attachment-load-op)
   (store-op . vk-attachment-store-op)
   (stencil-load-op . vk-attachment-load-op)
   (stencil-store-op . vk-attachment-store-op)
   (initial-layout . vk-image-layout)
   (final-layout . vk-image-layout)))

(define-foreign-struct vk-attachment-reference
  ((attachment . unsigned-32)
   (layout . vk-image-layout)))

(define-enum-ftype vk-pipeline-bind-point
  vk-pipeline-bind-point-graphics
  vk-pipeline-bind-point-compute
  (vk-pipeline-bind-point-ray-tracing-nv 1000165000))

(define-foreign-struct vk-subpass-description
  ((flags . flags)
   (pipeline-bind-point . vk-pipeline-bind-point)
   (input-attachment-count . unsigned-32)
   (input-attachments . (* vk-attachment-reference))
   (color-attachment-count . unsigned-32)
   (color-attachments . (* vk-attachment-reference))
   (resolve-attachments . (* vk-attachment-reference))
   (depth-stencil-attachments . (* vk-attachment-reference))
   (preserve-attachment-count . unsigned-32)
   (preserve-attachments . (* unsigned-32))))

(define-foreign-struct vk-subpass-dependency
  ((src-subpass . unsigned-32)
   (dest-subpass . unsigned-32)
   (src-stage-mask . flags)
   (dst-stage-mask . flags)
   (src-access-mask . flags)
   (dst-access-mask . flags)
   (dependency-flags . flags)))

(define-vulkan-struct vk-render-pass-create-info
  ((flags . flags)
   (attachment-count . unsigned-32)
   (attachments . (* vk-attachment-description))
   (subpass-count . unsigned-32)
   (subpasses . (* vk-subpass-description))
   (dependency-count . unsigned-32)
   (dependencies . (* vk-subpass-dependency))))

(define-vulkan-command vkCreateRenderPass
  ((& vk-device) (* vk-render-pass-create-info) uptr (* vk-render-pass)))


;; pipeline

(define-vulkan-struct vk-graphics-pipeline-create-info
  ((flags . flags)
   (stage-count . unsigned-32)
   (stages . (* vk-pipeline-shader-stage-create-info))
   (vertex-input-state . (* vk-pipeline-vertex-input-state-create-info))
   (input-assembly-state . (* vk-pipeline-input-assembly-state-create-info))
   (tessellation-stage . (* vk-pipeline-tessellation-state-create-info))
   (viewport-state . (* vk-pipeline-viewport-state-create-info))
   (rasterization-state . (* vk-pipeline-rasterization-state-create-info))
   (multisample-state . (* vk-pipeline-multisample-state-create-info))
   (depth-stencil-state . (* vk-pipeline-depth-stencil-state-create-info))
   (color-blend-state . (* vk-pipeline-color-blend-state-create-info))
   (dynamic-state . (* vk-pipeline-dynamic-state-create-info))
   (layout . vk-pipeline-layout)
   (render-pass . vk-render-pass)
   (subpass . unsigned-32)
   (base-pipeline-handle . vk-pipeline)
   (base-pipeline-index . int)))

(define-ftype vk-pipeline-cache uptr)

(define-vulkan-command vkCreateGraphicsPipelines
  ((& vk-device) uptr unsigned-32 (* vk-graphics-pipeline-create-info) uptr (* vk-pipeline)))

;; buffers

(define-ftype vk-buffer uptr)
(define-collection-lambdas vk-buffer)

(define-ftype vk-device-size unsigned-64)

(define-vulkan-struct vk-buffer-create-info
  ((flags . flags)
   (size . vk-device-size)
   (usage . flags)
   (sharing-mode . vk-sharing-mode)
   (queue-family-index-count . unsigned-32)
   (queue-family-indices . (* unsigned-32))))

(define-vulkan-command vkCreateBuffer
  ((& vk-device) (* vk-buffer-create-info) uptr (* vk-buffer)))

;; memory

(define +vk-max-memory-type+ 32)
(define +vk-max-memory-heaps+ 16)

(define-foreign-struct vk-memory-type
  ((property-flags . flags)
   (heap-index . u32)))

(define-foreign-struct vk-memory-heap
  ((size . vk-device-size)
   (memory-heap-flags . flags)))

(define-ftype vk-physical-device-memory-properties
  (struct (memory-type-count u32)
	  (memory-types (array 32 vk-memory-type))
	  (memory-heap-count u32)
	  (memory-heaps (array 16 vk-memory-heap))))




(define vk-get-physical-device-memory-properties
  (foreign-procedure "vkGetPhysicalDeviceMemoryProperties"
		     ((& vk-physical-device) (* vk-physical-device-memory-properties))
		     void))


(define-foreign-struct vk-memory-requirements
  ((size . vk-device-size)
   (alignment . vk-device-size)
   (memory-type-bits . unsigned-32)))

(define vk-get-image-memory-requirements
  (foreign-procedure "vkGetImageMemoryRequirements"
		     ((& vk-device) (& vk-image) (* vk-memory-requirements))
		     void))



(define vk-get-buffer-memory-requirements
  (foreign-procedure "vkGetBufferMemoryRequirements"
		     ((& vk-device) (& vk-buffer) (* vk-memory-requirements)) void))

;; allocate gpu memory

(define-ftype vk-device-memory uptr)

(define-vulkan-struct vk-memory-allocate-info
  ((allocation-size . vk-device-size)
   (memory-type-index . unsigned-32)))

(define-vulkan-command vkAllocateMemory
  ((& vk-device) (* vk-memory-allocate-info) uptr (* vk-device-memory)))

(define-vulkan-command vkBindBufferMemory
  ((& vk-device) (& vk-buffer) (& vk-device-memory) vk-device-size))

(define-vulkan-command vkBindImageMemory
  ((& vk-device) (& vk-image) (& vk-device-memory) vk-device-size))

(define-vulkan-command vkMapMemory
  ((& vk-device) (& vk-device-memory) vk-device-size vk-device-size flags uptr))

(define-vulkan-command vkUnmapMemory ((& vk-device) (& vk-device-memory)))

;; framebuffers
(define-ftype vk-frame-buffer uptr)

(define-vulkan-struct vk-frame-buffer-create-info
  ((flags . flags)
   (render-pass . vk-render-pass)
   (attachment-count . unsigned-32)
   (attachments . (* vk-image-view))
   (width . unsigned-32)
   (height . unsigned-32)
   (layers . unsigned-32)))

(define-vulkan-command vkCreateFramebuffer
  ((& vk-device) (* vk-frame-buffer-create-info) uptr (* vk-frame-buffer)))

;; command pool
(define-ftype vk-command-pool uptr)

(define-vulkan-struct vk-command-pool-create-info
  ((flags . flags)
   (queue-family-index . unsigned-32)))

(define-vulkan-command vkCreateCommandPool
  ((& vk-device) (* vk-command-pool-create-info) uptr (* vk-command-pool)))

(define-vulkan-command vkDestroyCommandPool ((& vk-device) (& vk-command-pool) uptr))

(define-vulkan-command vkResetCommandPool ((& vk-device) (& vk-command-pool) flags))



;; Command buffers
(define-ftype vk-command-buffer uptr)
(define-collection-lambdas vk-command-buffer)

(define-enum-ftype vk-command-buffer-level
  vk-command-buffer-level-primary
  vk-command-buffer-level-secondary)

(define-vulkan-struct vk-command-buffer-allocate-info
  ((command-pool . vk-command-pool)
   (level . vk-command-buffer-level)
   (command-buffer-count . unsigned-32)))

(define-vulkan-command vkAllocateCommandBuffers
  ((& vk-device) (* vk-command-buffer-allocate-info) (* vk-command-buffer)))

(define-vulkan-command vkFreeCommandBuffers
  ((& vk-device) (& vk-command-pool) unsigned-32 (* vk-command-buffer)))

(define-vulkan-struct vk-command-buffer-inheritance-info
  ((render-pass . vk-render-pass)
   (subpass . unsigned-32)
   (framebuffer . vk-frame-buffer)
   (occlusion-query-enable . vk-bool32)
   (query-flags . flags)
   (pipeline-statistic-flags . flags)))

(define-vulkan-struct vk-command-buffer-begin-info
  ((flags . flags)
   (inheritance-info . (* vk-command-buffer-inheritance-info))))

(define-vulkan-command vkBeginCommandBuffer
  ((& vk-command-buffer) (* vk-command-buffer-begin-info)))
(define-vulkan-command vkEndCommandBuffer ((& vk-command-buffer)))
(define-vulkan-command vkResetCommandBuffer ((& vk-command-buffer) flags))

(define-enum-ftype vk-index-type
  vk-index-type-uint16
  vk-index-type-uint32
  (vk-index-type-none-nv 1000165000)
  (vk-index-type-uint8-ext 1000265000))



;; macro to help define render pass commands
(define-syntax define-render-pass-command
  (lambda (stx)
    (syntax-case stx ()
      ((_ scheme-cmd) #'(define-render-pass-command scheme-cmd ()))
      ((_ scheme-cmd (args ...))
       (with-syntax ((c-command
		      (datum->syntax #'scheme-cmd
				     (kebab-case->camel-case
				      (symbol->string (syntax->datum #'scheme-cmd))))))
	 #'(define scheme-cmd
	     (foreign-procedure c-command
				((& vk-command-buffer) args ...)
				void)))))))


(define-ftype vk-descriptor-set uptr)
(define-collection-lambdas vk-descriptor-set)

;; render pass begin info

;; clear values
(define-ftype vk-clear-color-value
  (union (float32 (array 4 single-float))
	 (int32 (array 4 integer-32))
	 (uint32 (array 4 u32))))

(define make-vk-clear-color-value
  (lambda (values)
    (let ((ptr (make-foreign-object vk-clear-color-value)))
      (for-each (lambda (v)
		  (ftype-set! vk-clear-color-value (float32 0) ptr v)
		  (ftype-set! vk-clear-color-value (float32 1) ptr v)
		  (ftype-set! vk-clear-color-value (float32 2) ptr v)
		  (ftype-set! vk-clear-color-value (float32 3) ptr v))
		values)
      ptr)))

(define-foreign-struct vk-clear-depth-stencil-value
  ((depth . float)
   (stencil . u32)))

(define-ftype vk-clear-value
  (union (color vk-clear-color-value)
	 (depth-stencil vk-clear-depth-stencil-value)))

(define-collection-lambdas vk-clear-value)

(define make-vk-clear-value
  (lambda (clear-values)
    (let ((clear-value-ptr (make-foreign-object vk-clear-value)))
      (exclusive-cond
       ((list? clear-values)
	(map (lambda (v i)
	       (ftype-set! vk-clear-value (color float32 i) clear-value-ptr v))
	     clear-values
	     (iota (length clear-values))))
       ((pair? clear-values)
	(ftype-set! vk-clear-value
		    (depth-stencil depth)
		    clear-value-ptr
		    (car clear-values))
	(ftype-set! vk-clear-value
		    (depth-stencil stencil)
		    clear-value-ptr
		    (cdr clear-values))))
      clear-value-ptr)))


(define-ftype vk-render-pass-begin-info
  (struct (s-type vk-structure-type)
	  (p-next uptr)
	  (render-pass vk-render-pass)
	  (framebuffer vk-frame-buffer)
	  (render-area vk-rect-2d)
	  (clear-value-count u32)
	  (clear-values (* vk-clear-value))))

;; offset cons cell (x . y)
;; extent cons cell (width . height)
(define-record-type render-area (fields offset extent))

(define make-vk-render-pass-begin-info
  (lambda (render-pass framebuffer render-area clear-values)
    (match render-area
      (($ render-area offset extent)
       (let ((info (make-foreign-object vk-render-pass-begin-info)))
	 (ftype-set! vk-render-pass-begin-info (s-type) info render-pass-begin-info)
	 (ftype-set! vk-render-pass-begin-info (p-next) info 0)
	 (ftype-set! vk-render-pass-begin-info
		     (render-pass)
		     info
		     (pointer-ref-value render-pass))
	 (ftype-set! vk-render-pass-begin-info
		     (framebuffer)
		     info
		     (pointer-ref-value framebuffer))
	 (ftype-set! vk-render-pass-begin-info (render-area offset x) info (car offset))
	 (ftype-set! vk-render-pass-begin-info (render-area offset y) info (cdr offset))
	 (ftype-set! vk-render-pass-begin-info (render-area extent width) info (car extent))
	 (ftype-set! vk-render-pass-begin-info (render-area extent height) info (cdr extent))
	 (ftype-set! vk-render-pass-begin-info
		     (clear-value-count)
		     info
		     (array-pointer-length clear-values))
	 (ftype-set! vk-render-pass-begin-info
		     (clear-values)
		     info
		     (array-pointer-raw-ptr clear-values))
	 info)))))


(define-enum-ftype vk-subpass-contents
  (vk-subpass-contents-inline  0)
  (vk-subpass-contents-secondary-command-buffers  1))

;; render pass commands

(define-render-pass-command vk-cmd-bind-pipeline (vk-pipeline-bind-point (& vk-pipeline)))

(define-render-pass-command vk-cmd-bind-vertex-buffers
  (unsigned-32 unsigned-32 (* vk-buffer) (* vk-device-size)))

(define-render-pass-command vk-cmd-bind-index-buffer
  ((& vk-buffer) vk-device-size vk-index-type))

(define-render-pass-command vk-cmd-bind-descriptor-sets
  (vk-pipeline-bind-point (& vk-pipeline-layout) unsigned-32 unsigned-32
			  (* vk-descriptor-set) unsigned-32 (* unsigned-32)))

(define-render-pass-command vk-cmd-pipeline-barrier
  (flags flags flags u32 uptr u32 uptr u32 (* vk-image-memory-barrier)))

(define-render-pass-command vk-cmd-draw (u32 u32 u32 u32))

(define-render-pass-command vk-cmd-draw-indexed
  (unsigned-32 unsigned-32 unsigned-32 int unsigned-32))

(define-render-pass-command vk-cmd-begin-render-pass
  ((* vk-render-pass-begin-info) vk-subpass-contents))

(define-render-pass-command vk-cmd-end-render-pass)

(define-foreign-struct vk-buffer-copy
  ((src-offset . vk-device-size)
   (dst-offset . vk-device-size)
   (size . vk-device-size)))

(define-render-pass-command vk-cmd-copy-buffer ((& vk-buffer) (& vk-image) u32 (* vk-buffer-copy)))

(define-foreign-struct vk-image-subresource-layers
  ((aspect-mask . flags)
   (mip-level . u32)
   (base-array-layer . u32)
   (layer-count . u32)))

(define-foreign-struct vk-buffer-image-copy
  ((buffer-offset . vk-device-size)
   (buffer-row-length . u32)
   (buffer-image-height . u32)
   (image-subresource . vk-image-subresource-layers)
   (image-offset . vk-offset-3d)
   (image-extent . vk-extent-3d)))


(define-render-pass-command vk-cmd-copy-buffer-to-image
  ((& vk-buffer) (& vk-image) vk-image-layout u32 (* vk-buffer-image-copy)))

;; ftypes to submit command buffer to queue

(define-enum-ftype vk-filter
  vk-filter-nearest
  vk-filter-linear
  (vk-filter-cubic-img 1000015000))

(define-enum-ftype vk-border-color
  (vk-border-color-float-transparent-black  0)
  (vk-border-color-int-transparent-black  1)
  (vk-border-color-float-opaque-black  2)
  (vk-border-color-int-opaque-black  3)
  (vk-border-color-float-opaque-white  4)
  (vk-border-color-int-opaque-white  5))

(define-enum-ftype vk-sampler-address-mode
  (vk-sampler-address-mode-repeat  0)
  (vk-sampler-address-mode-mirrored-repeat  1)
  (vk-sampler-address-mode-clamp-to-edge  2)
  (vk-sampler-address-mode-clamp-to-border  3)
  (vk-sampler-address-mode-mirror-clamp-to-edge  4))

(define-enum-ftype vk-sampler-mipmap-mode
  vk-sampler-mipmap-mode-nearest vk-sampler-mipmap-mode-linear)

(define-vulkan-struct vk-sampler-create-info
  ((flags . flags)
   (mag-filter . vk-filter)
   (min-filter . vk-filter)
   (mip-map-mode . vk-sampler-mipmap-mode)
   (address-mode-u . vk-sampler-address-mode)
   (address-mode-v . vk-sampler-address-mode)
   (address-mode-w . vk-sampler-address-mode)
   (mip-load-bias . single-float)
   (anisotropy-enable . vk-bool32)
   (max-anisotropy . single-float)
   (compare-enable . vk-bool32)
   (compare-op . vk-compare-op)
   (min-lod . single-float)
   (max-load . single-float)
   (border-color . vk-border-color)
   (unnormalized-coordinates . vk-bool32)))


(define-vulkan-command vkCreateSampler
  ((& vk-device) (* vk-sampler-create-info) uptr (* vk-sampler)))



(define-ftype vk-semaphore uptr)
(define-ftype vk-fence uptr)

(define-vulkan-struct vk-submit-info
  ((wait-semaphore-count . u32)
   (wait-semaphores . (* vk-semaphore))
   (wait-dst-stage-mask . (* flags))
   (command-buffer-count . u32)
   (command-buffers . (* vk-command-buffer))
   (signal-semaphore-count . u32)
   (signal-semaphores . (* vk-semaphore))))

(define-vulkan-command vkQueueSubmit ((& vk-queue) u32 (* vk-submit-info) uptr))

(define-vulkan-command vkQueueWaitIdle ((& vk-queue)))

(define-vulkan-command vkDeviceWaitIdle ((& vk-device)))


;; descriptor pool
(define-ftype vk-descriptor-pool uptr)

(define-foreign-struct vk-descriptor-pool-size
  ((type . vk-descriptor-type)
   (descriptor-count . u32)))

(define-collection-lambdas vk-descriptor-pool-size)

(define-vulkan-struct vk-descriptor-pool-create-info
  ((flags . flags)
   (max-sets . u32)
   (pool-size-count . u32)
   (pool-sizes . (* vk-descriptor-pool-size))))

(define-vulkan-command vkCreateDescriptorPool
  ((& vk-device) (* vk-descriptor-pool-create-info) uptr (* vk-descriptor-pool)))

;; descriptor sets


(define-vulkan-struct vk-descriptor-set-allocate-info
  ((descriptor-pool . vk-descriptor-pool)
   (descriptor-set-count . u32)
   (set-layouts . (* vk-descriptor-set-layout))))

(define-vulkan-command vkAllocateDescriptorSets
  ((& vk-device) (* vk-descriptor-set-allocate-info) (* vk-descriptor-set)))

;; update descriptor sets

(define-ftype vk-buffer-view uptr)

(define-foreign-struct vk-descriptor-image-info
  ((sampler . vk-sampler)
   (image-view . vk-image-view)
   (image-layout . vk-image-layout)))

(define-foreign-struct vk-descriptor-buffer-info
  ((buffer . vk-buffer)
   (offset . vk-device-size)
   (range . vk-device-size)))

(define-vulkan-struct vk-write-descriptor-set
  ((dst-set . vk-descriptor-set)
   (dst-binding . u32)
   (dst-array-element . u32)
   (descriptor-count . u32)
   (descriptor-type . vk-descriptor-type)
   (image-info . (* vk-descriptor-image-info))
   (buffer-info . (* vk-descriptor-buffer-info))
   (texel-buffer-view . (* vk-buffer-view))))

(define-collection-lambdas vk-write-descriptor-set)

(define-vulkan-command vkUpdateDescriptorSets
  ((& vk-device) u32 (* vk-write-descriptor-set) u32 uptr))

;; depth buffering

(define-foreign-struct vk-format-properties
  ((linear-tiling-features . flags)
   (optimal-tiling-features . flags)
   (buffer-features . flags)))

(define vk-get-physical-device-format-properties
  (foreign-procedure "vkGetPhysicalDeviceFormatProperties"
		     ((& vk-physical-device) vk-format (* vk-format-properties))
		     void))

;; synchronization utilities

;; semaphores

(define-vulkan-struct vk-semaphore-create-info
  ((flags . flags)))

(define-vulkan-command vkCreateSemaphore
  ((& vk-device) (* vk-semaphore-create-info) uptr (* vk-semaphore)))

(define-vulkan-command vkDestroySemaphore ((& vk-device) (& vk-semaphore) uptr))

;; fences

(define-enum-ftype vk-fence-create-flag-bits
  (vk-fence-create-signaled-bit  #x00000001))

(define-vulkan-struct vk-fence-create-info
  ((flags . flags)))

(define-vulkan-command vkCreateFence
  ((& vk-device) (* vk-fence-create-info) uptr (* vk-fence)))

(define-vulkan-command vkDestroyFence ((& vk-device) (& vk-fence) uptr))
(define-vulkan-command vkResetFences ((& vk-device) u32 (* vk-fence)))
(define-vulkan-command vkGetFenceStatus ((& vk-device) (& vk-fence)))
(define-vulkan-command vkWaitForFences ((& vk-device) u32 (* vk-fence) vk-bool32 u64))

(define-vulkan-command vkAcquireNextImageKHR
  ((& vk-device) (& vk-swapchain) u64 (& vk-semaphore) uptr (* u32)))

(define-vulkan-struct vk-present-info
  ((wait-semaphore-count . u32)
   (wait-semaphores . (* vk-semaphore))
   (swapchain-count . u32)
   (swapchains . (* vk-swapchain))
   (image-indices . (* u32))
   (results . (* int))))

(define-vulkan-command vkQueuePresentKHR ((& vk-queue) (* vk-present-info)))

#!eof

--------------------------------------------

(import (ffi)
	(vulkan structure-types))
(load "vulkan/enums.scm")
(load "vulkan/ftype.scm")

(define v (load-shared-object "libvulkan.so.1"))

(library-directories '("./thunderchez" "."))
