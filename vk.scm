;; (load "vulkan/instance.scm")
;; (load "vulkan/structure-types.scm")
;; (load "ffi.scm")

(import (ffi))
(import (glfw))
(import (vulkan instance))
(import (vulkan structure-types))
(import (vulkan))

(define validation? #t)

(trace make-vulkan-instance)

(define instance (make-vulkan-instance #t))

(ftype-pointer-ftype  instance)

(create-debug-utils-messenger instance)

(ptr->strings (get-required-extensions))

(get-procedure-address instance "vkEnumeratePhysicalDevices")

(define count (make-foreign-object int))

(define strs (make-foreign-object uptr))

(define layers (make-ftype-pointer uptr (17 * )))

(define c (make-foreign-object int))
((foreign-procedure "vkEnumeratePhysicalDevices" ((& vk-instance) (* int) uptr) int)
 instance c 0)

(ftype-pointer->sexpr c)

(_enumerate-physical-devices instance
			     count
			     0)


(trace-define-syntax define-enum-library
		     (lambda (stx)
		       (define (identifiers e*)
			 (let lp ((e* e*)
				  (ids '()))
			   (cond
			    ((null? e*) ids)
			    (else 
			     (syntax-case (car e*) ()
			       ((e _) (lp (cdr e*)
					  (cons #'e ids)))
			       (e (lp (cdr e*)
				      (cons #'e ids))))))))
		       
		       (syntax-case stx ()
			 ((_ library-name enum-name e* ...)
			  (with-syntax (((e ...) (identifiers #'(e* ...))))
			    (syntax (library library-name
				      (export e ...)
				      (import (ffi))
				      (define-enum-ftype enum-name e* ...))))))))


(trace-define-syntax define-vulkan-enum
  (syntax-rules ()
    ((_ enum-name e* ...)
     (define-enum-library (vulkan enum-name) enum-name e* ...))))


(define-vulkan-enum vk-result vk-false vk-true)
