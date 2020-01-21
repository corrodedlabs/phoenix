(library (glfw)
  (export glfw-init
	  glfw-get-required-instance-extensions
	  create-window)

  (import (chezscheme)
	  (ffi)
	  (glfw glfw))

  ;; (define glfw (load-shared-object "libglfw.so"))

  (define glfw-init (foreign-procedure "glfwInit" () boolean))

  ;; create glfw window also initializes glfw
  (define create-window
    (lambda (width height)
      (glfwInit)
      (glfwWindowHint GLFW_CLIENT_API GLFW_NO_API)
      (glfwWindowHint GLFW_RESIZABLE GLFW_FALSE)
      (glfwCreateWindow width height "Phoenix" 0 0)))
  
  (define glfw-get-required-instance-extensions
    (lambda ()
      (let ((f (foreign-procedure "glfwGetRequiredInstanceExtensions" ((* int)) uptr)))
	(let* ((num-extensions (make-foreign-object int))
	       (extensions (f num-extensions)))
	  (cons (read-int num-extensions)
		extensions))))))

#|

> (load "glfw.scm")
> (import (glfw))

> (glfw-init)

> (ptr->strings (glfw-get-required-instance-extensions))

|#
