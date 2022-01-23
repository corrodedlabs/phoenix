#!/usr/bin/scheme
;; (load "vulkan/instance.scm")
;; (load "vulkan/structure-types.scm")
;; (load "ffi.scm")

(library-directories '("./thunderchez" "."))

(import (prelude)

	(ffi)
	(assimp)
	(glfw)
	(shaderc)
	(camera)
	(image)
	;; (renderdoc)

	(srfi s43 vectors)

	(vulkan structure-types)
	(matchable))




(begin (glfw-init)
       (define v (load-shared-object "libvulkan.so.1"))

       (load "vulkan/enums.scm")
       (load "vulkan/ftype.scm")
       (load "vulkan/instance.scm")
       (load "vulkan/surface.scm")

       (load "vulkan/queues.scm")
       (load "vulkan/devices.scm")
       (load "vulkan/swapchain.scm"))

(define-record-type vulkan-state
  (fields window surface physical-device queue-index device queues swapchain))


(define setup-vulkan
  (lambda ()
    (let* ((instance (init-vulkan))
	   (_ (glfw-init))
	   (window  (setup-window instance 3840 2160))
	   (surface (window-details-surface window))
	   (physical-device (array-pointer-raw-ptr (get-physical-devices instance)))
	   (queue-index (find-queue-family physical-device surface))
	   (device (create-logical-device physical-device queue-index))
	   (queues (create-queue-handles device))
	   (swapchain-details (create-swapchain physical-device device surface queue-index)))
      (make-vulkan-state window surface physical-device queue-index device queues
			 swapchain-details))))

(define vs (setup-vulkan))
(load "vulkan/images.scm")
(load "vulkan/pipeline.scm")
(load "vulkan/texture.scm")
(load "vulkan/buffers.scm")

;; ;; (start-frame-capture)

;; (load "vulkan/sync.scm")

(define window (window-details-window (vulkan-state-window vs)))
(start-loop)

#!eof

> (load "vk.scm")

> (load "ffi.scm")
> (import (ffi))
> (load "vulkan/ftype.scm")

> (define vs (setup-vulkan))




;; (define validation? #t)

;; (trace make-vulkan-instance)

;; (define instance (make-vulkan-instance #t))

;; (ftype-pointer-ftype  instance)

;; (create-debug-utils-messenger instance)

;; (ptr->strings (get-required-extensions))

;; (get-procedure-address instance "vkEnumeratePhysicalDevices")

;; (define count (make-foreign-object int))

;; (define strs (make-foreign-object uptr))

;; (define layers (make-ftype-pointer uptr (17 * )))

;; (define c (make-foreign-object int))
;; ((foreign-procedure "vkEnumeratePhysicalDevices" ((& vk-instance) (* int) uptr) int)
;;  instance c 0)

;; (ftype-pointer->sexpr c)

;; (_enumerate-physical-devices instance
;; 			     count
;; 			     0)

;; (get-physical-devices instance)

;; (trace-define-syntax define-enum-library
;; 		     (lambda (stx)
;; 		       (define (identifiers e*)
;; 			 (let lp ((e* e*)
;; 				  (ids '()))
;; 			   (cond
;; 			    ((null? e*) ids)
;; 			    (else 
;; 			     (syntax-case (car e*) ()
;; 			       ((e _) (lp (cdr e*)
;; 					  (cons #'e ids)))
;; 			       (e (lp (cdr e*)
;; 				      (cons #'e ids))))))))

;; 		       (syntax-case stx ()
;; 			 ((_ library-name enum-name e* ...)
;; 			  (with-syntax (((e ...) (identifiers #'(e* ...))))
;; 			    (syntax (library library-name
;; 				      (export e ...)
;; 				      (import (ffi))
;; 				      (define-enum-ftype enum-name e* ...))))))))


;; (trace-define-syntax define-vulkan-enum
;;   (syntax-rules ()
;;     ((_ enum-name e* ...)
;;      (define-enum-library (vulkan enum-name) enum-name e* ...))))


;; (define-vulkan-enum vk-result vk-false vk-true)
