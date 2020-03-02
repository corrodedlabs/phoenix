(library (glfw)
  (export glfw-init
	  glfw-get-required-instance-extensions
	  create-window
	  new-window)

  (import (chezscheme)
	  (ffi)
	  (glfw glfw))

  ;; (define glfw (load-shared-object "libglfw.so"))

  (define glfw-init (foreign-procedure "glfwInit" () boolean))

  (define-record-type window (fields handle surface))

  ;; create glfw window also initializes glfw
  (define new-window
    (lambda (width height)
      (glfwInit)
      (glfwWindowHint GLFW_CLIENT_API GLFW_NO_API)
      (glfwWindowHint GLFW_RESIZABLE GLFW_FALSE)
      (glfwCreateWindow width height "Phoenix" 0 0)))

  (define create-surface
    (lambda (vk-instance window)
      (let ((surface (make-foreign-object uptr)))
	(case (glfwCreateWindowSurface (ftype-pointer-address vk-instance)
				       window
				       0
				       (ftype-pointer-address surface))
	  ((0) surface)
	  (else (error "failed to create window surface" window))))))

  (define create-window
    (lambda (vk-instance width height)
      (let ((window (new-window width height)))
	(make-window window (create-surface vk-instance window)))))
  
  (define glfw-get-required-instance-extensions
    (lambda ()
      (let ((f (foreign-procedure "glfwGetRequiredInstanceExtensions"
				  ((* int)) uptr)))
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
