(library (glfw)
  (export glfw-init
	  glfw-get-required-instance-extensions)

  (import (chezscheme)
	  (ffi))

  (define glfw (load-shared-object "libglfw.so"))

  (define glfw-init (foreign-procedure "glfwInit" () boolean))
  
  (define glfw-get-required-instance-extensions
    (lambda ()
      (let ((f (foreign-procedure "glfwGetRequiredInstanceExtensions" ((* int)) (* char**))))
	(let* ((num-extensions (make-foreign-object int))
	       (extensions (f num-extensions)))
	  (cons (read-int num-extensions) extensions))))))

#|

> (import (glfw))

> (glfw-init)

> (glfw-get-required-instance-extensions)

|#
