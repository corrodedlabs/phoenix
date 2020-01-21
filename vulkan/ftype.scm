

(define-syntax define-vulkan-struct
  (syntax-rules ()
    ((_ struct-name ((member-name . member-type) ...))
     (define-foreign-struct struct-name
       [(sType . vk-structure-type)
	(next  . uptr)
	(member-name . member-type) ...]))))

(define-ftype uint32-t unsigned-32)

(define-ftype flags uint32-t)

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


(define-syntax callback
  (syntax-rules ()
    ((_ f (<args> ...) <ret>)
     (let ([code (foreign-callable f (<args> ...) <ret>)])
       (lock-object code)
       (foreign-callable-entry-point code)))))

(define-ftype vk-instance (struct (instance uptr)))

;; physical device

(define-ftype vk-physical-device (struct (physical-device uptr)))

(define _enumerate-physical-devices
  (foreign-procedure "vkEnumeratePhysicalDevices"
		     ((& vk-instance) (* unsigned-32) (* vk-physical-device)) int))

(define camel-case->kebab-case
  (lambda (str)
    (list->string
     (apply append (map (lambda (ch)
			  (cond
			   ((char-upper-case? ch) (list #\- (char-downcase ch)))
			   (else (list ch))))
			(string->list str))))))

(trace-define-syntax define-vulkan-command
  (lambda (stx)
    (syntax-case stx ()
      [(_ command (argument-types ...) return-type)
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
		      (foreign-procedure command-name (argument-types ...) return-type))

		    (define ffi-proc
		      (lambda (arg-names ...)
			(case (_ffi-proc arg-names ...)
			  ((0) #t)
			  (else (error "vulkan command failed" command-name))))))))])))

(define-vulkan-command
  vkEnumeratePhysicalDevices
  ((& vk-instance) (* unsigned-32) (* vk-physical-device)) int)

#!eof

(import (ffi)
	(vulkan structure-types)
	(srfi 13))

(load "vulkan/ftype.scm")

(define v (load-shared-object "libvulkan.so.1"))

(library-directories '("./thunderchez" "."))
