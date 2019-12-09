(library (glfw)
  (export glfw-init
	  glfw-get-required-instance-extensions)

  (import (chezscheme)
	  (ffi))

  (define glfw (load-shared-object "libglfw.so"))

  (define-ftype char** (* (* char)))

  (define glfw-init (foreign-procedure "glfwInit" () boolean))

  (define (char**->strings ptr count)
    (let lp ((i 0)
	     (strs '()))
      (cond
       ((< i count)
	(lp (+ 1 i)
	    (cons (ptr->string (ftype-ref char** () ptr i)) strs)))
       (else strs))))

  (define-syntax read-int
    (syntax-rules ()
      ((_ ptr) (ftype-ref int () ptr))))
  
  (define glfw-get-required-instance-extensions
    (lambda ()
      (let ((f (foreign-procedure "glfwGetRequiredInstanceExtensions" ((* int)) (* char**))))
	(let* ((num-extensions (make-foreign-object int))
	       (extensions (f num-extensions)))
	  (char**->strings extensions (read-int num-extensions)))))))

#!eof

> (import (glfw))

> (glfw-init)

> (glfw-get-required-instance-extensions)
