#! /usr/bin/scheme

(library-directories '("./thunderchez" "./chez-gl/lib" "."))

(import (glfw)
	(glfw glfw)
	(glfw window)
	(gl)
	(gl ftype)
	(ffi)
	(image)
	(prelude)
	(math)
	(matchable)
	(camera))

(define window (new-opengl-window 3840 2160))

(glfwMakeContextCurrent window)

(gl-init)
(gl-viewport 0 0 3840 2160)

(glEnable GL-DEPTH-TEST)

(glfwSetInputMode window GLFW_CURSOR GLFW_CURSOR_DISABLED)

(define gl-texture (make-foreign-object unsigned))

(glGenTextures 1 gl-texture)
(glBindTexture GL-TEXTURE-2D (read-unsigned gl-texture))
(glTexParameteri GL-TEXTURE-2D GL-TEXTURE-WRAP-S GL-REPEAT)
(glTexParameteri GL-TEXTURE-2D GL-TEXTURE-WRAP-T GL-REPEAT)

(glTexParameteri GL-TEXTURE-2D GL-TEXTURE-MIN-FILTER GL-LINEAR)
(glTexParameteri GL-TEXTURE-2D GL-TEXTURE-MAG-FILTER GL-LINEAR)


(with-image "textures/winter.jpeg"
	    (lambda (image)
	      (glTexImage2D GL-TEXTURE-2D
			    0
			    GL-RGBA
			    (image-data-width image)
			    (image-data-height image)
			    0
			    GL-RGBA
			    GL-UNSIGNED-BYTE
			    (image-data-pointer image))
	      (display "errpr: ")
	      (displayln (glGetError))
	      (glGenerateMipmap GL-TEXTURE-2D)))




;;
(define vertex-shader-source
"#version 330 core

layout (location = 0) in vec3 pos;
layout (location = 1) in vec2 inTexCoord;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec2 texCoord;

void main()
{
  gl_Position = projection * view * model * vec4(pos.x, pos.y, pos.z, 1.0);
  texCoord = vec2(inTexCoord.x, inTexCoord.y);
}")

(define fragment-shader-source
"#version 330 core
in vec2 texCoord;
uniform sampler2D tex;
out vec4 frag;

void main()
{
    frag = texture(tex, texCoord);
}")

(define gl-vertex-shader   (gl-create-shader GL-VERTEX-SHADER))
(define gl-fragment-shader (gl-create-shader GL-FRAGMENT-SHADER))
(define gl-shader-program  (gl-create-program))

(gl-shader-source gl-vertex-shader vertex-shader-source)
(gl-compile-shader gl-vertex-shader)

(gl-shader-source gl-fragment-shader fragment-shader-source)
(gl-compile-shader gl-fragment-shader)

(gl-attach-shader gl-shader-program gl-vertex-shader)
(gl-attach-shader gl-shader-program gl-fragment-shader)
(gl-link-program gl-shader-program)
(gl-delete-shader gl-vertex-shader)
(gl-delete-shader gl-fragment-shader)
(gl-use-program gl-shader-program)





;;
(define gl-vao (gl-gen-vertex-arrays 1))
(define gl-vbo (gl-gen-buffers 1))
(define gl-ebo (gl-gen-buffers 1))

(gl-bind-vertex-array gl-vao)

(gl-bind-buffer GL-ARRAY-BUFFER gl-vbo)
(gl-buffer-data GL-ARRAY-BUFFER
		'(-0.5 -0.5 -0.5 0.0 0.0
		       0.5 -0.5 -0.5  1.0 0.0
		       0.5  0.5 -0.5  1.0 1.0
		       0.5  0.5 -0.5  1.0 1.0
		       -0.5  0.5 -0.5  0.0 1.0
		       -0.5 -0.5 -0.5  0.0 0.0

		       -0.5 -0.5  0.5  0.0 0.0
		       0.5 -0.5  0.5  1.0 0.0
		       0.5  0.5  0.5  1.0 1.0
		       0.5  0.5  0.5  1.0 1.0
		       -0.5  0.5  0.5  0.0 1.0
		       -0.5 -0.5  0.5  0.0 0.0

		       -0.5  0.5  0.5  1.0 0.0
		       -0.5  0.5 -0.5  1.0 1.0
		       -0.5 -0.5 -0.5  0.0 1.0
		       -0.5 -0.5 -0.5  0.0 1.0
		       -0.5 -0.5  0.5  0.0 0.0
		       -0.5  0.5  0.5  1.0 0.0

		       0.5  0.5  0.5  1.0 0.0
		       0.5  0.5 -0.5  1.0 1.0
		       0.5 -0.5 -0.5  0.0 1.0
		       0.5 -0.5 -0.5  0.0 1.0
		       0.5 -0.5  0.5  0.0 0.0
		       0.5  0.5  0.5  1.0 0.0

		       -0.5 -0.5 -0.5  0.0 1.0
		       0.5 -0.5 -0.5  1.0 1.0
		       0.5 -0.5  0.5  1.0 0.0
		       0.5 -0.5  0.5  1.0 0.0
		       -0.5 -0.5  0.5  0.0 0.0
		       -0.5 -0.5 -0.5  0.0 1.0

		       -0.5  0.5 -0.5  0.0 1.0
		       0.5  0.5 -0.5  1.0 1.0
		       0.5  0.5  0.5  1.0 0.0
		       0.5  0.5  0.5  1.0 0.0
		       -0.5  0.5  0.5  0.0 0.0
		       -0.5  0.5 -0.5  0.0 1.0)
		GL-STATIC-DRAW)

;; (gl-bind-buffer GL-ELEMENT-ARRAY-BUFFER gl-ebo)
;; (gl-buffer-data GL-ELEMENT-ARRAY-BUFFER
;; 		(uint-list->bytevector '(0 1 3 1 2 3)
;; 				       'little
;; 				       4)
;; 		GL-STATIC-DRAW)

(gl-vertex-attrib-pointer 0 3 GL-FLOAT GL-FALSE (* 5 4) 0)
(gl-enable-vertex-attrib-array 0)

(gl-vertex-attrib-pointer 1
			  2
			  GL-FLOAT
			  GL-FALSE
			  (* 5 4)
			  (* 3 4))
(gl-enable-vertex-attrib-array 1)

(glUniform1i (glGetUniformLocation gl-shader-program "tex") 0)


(define-collection-lambdas float)

(define set-uniform-matrix!
  (lambda (matrix uniform-variable)
    (glUniformMatrix4fv (glGetUniformLocation gl-shader-program uniform-variable)
			1
			GL-FALSE
			(array-pointer-raw-ptr
			 (list->float-pointer-array
			  (map-indexed (lambda (value i)
					 (let ((ptr (make-foreign-object float)))
					   (ftype-set! float () ptr value)
					   ptr))
				       (matrix->list matrix)))))))

(define cube-positions
  (list (make-vector3 0.0 0.0 0.0)
	(make-vector3 2.0 5.0 -15.0)
	(make-vector3 -1.5 -2.2 -2.5)
	(make-vector3 -3.8 -2.0 -12.3)
	(make-vector3 2.4 -0.4 -3.5)
	(make-vector3 -1.7 3.0 7.5)
	(make-vector3 1.3 -2.0 -2.5)
	(make-vector3 1.5 2.0 -2.5)
	(make-vector3 1.5 0.2 -1.5)
	(make-vector3 -1.3 1.0 -1.5)))

(define rotate-camera-view
  (lambda ()
    (let* ((radius 10.0)
	   (camx (* radius (sin (glfwGetTime))))
	   (camz (* radius (cos (glfwGetTime)))))
      (look-at (make-vector3 camx 0.0 camz)
	       (make-vector3 0.0 0.0 0.0)
	       (make-vector3 0.0 1.0 0.0)))))

(define camera->view
  (lambda (cam)
    (match-let ((($ camera position front up) cam))
      (look-at position
	       (vector3+ position front)
	       up))))

(define-record-type game-state
  (nongenerative game-state)
  (fields camera last-render-time should-close?))

;; (define *camera-front* (camera-front (default-camera)))

(define *mouse-stream* '(1920.0 . 1080.0))

(define mouse-callback
  (lambda (window xpos ypos)
    (set! *mouse-stream* (cons xpos ypos))))

(define *fov* 45.0)

(define scroll-callback
  (lambda (window x-offset y-offset)
    (set! *fov*
	  (clamp-between 1.0 (- *fov* y-offset) 125.0))))

(define process-input
  (lambda (window state)
    (define get-input-directions
      (lambda ()
	(map cdr
	     (filter
	      (lambda (key)
		(= GLFW_PRESS (glfwGetKey window (car key))))
	      (map cons
		   (list GLFW_KEY_W GLFW_KEY_A GLFW_KEY_S GLFW_KEY_D)
		   (list forward    left       back       right))))))
    
    (if (= GLFW_PRESS (glfwGetKey window GLFW_KEY_Q))
	(make-game-state (game-state-camera state)
			 (game-state-last-render-time state)
			 #t)
	(let* ((t0 (game-state-last-render-time state))
	       (t1 (glfwGetTime))
	       (input-directions (get-input-directions))
	      ;;; todo make update-camera have separate api
	       ;; for rotation
	       (cam (update-camera (game-state-camera state)
				   #f
				   (- t1 t0)
				   *mouse-stream*)))
	  (if (null? input-directions)
	      (make-game-state cam t1 #f)
	      (fold-left
	       (lambda (state direction)
		 (make-game-state (update-camera cam
						 direction
						 (- t1 t0)
						 *mouse-stream*) t1 #f))
	       state
	       input-directions))))))
;; fps
;; (define last (glfwGetTime))
;; (define frames 0)
;; (set! frames (+ 1 frames))
;; (when (< 1 (- (glfwGetTime) last))
;;   (display "fps: ")
;;   (displayln frames)
;;   (set! frames 0)
;;   (set! last (glfwGetTime)))


(define draw-frame
  (lambda (state)
    (gl-clear-color 0.0 0.5 0.0 1.0)
    (gl-clear (bitwise-ior GL-COLOR-BUFFER-BIT
			   GL-DEPTH-BUFFER-BIT))
    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL-TEXTURE-2D
		   (read-unsigned gl-texture))


    (let ((model (rotate-matrix (identity-matrix4)
				(fl* (glfwGetTime)
				     (degree->radian 50.0))
				(make-vector3 0.5 1.0 0.0)))
	  ;; (view (translate-matrix (identity-matrix4)
	  ;; 			  (make-vector3 0.0 0.0 -3.0)))
	  (view (camera->view (game-state-camera state)))
	  ;; (view (rotate-camera-view))
	  (projection (opengl-perspective-matrix *fov*
						 (/ 3840 2160)
						 0.1
						 100.0)))
      (set-uniform-matrix! view "view")
      (set-uniform-matrix! projection "projection"))

    
    
    (gl-use-program gl-shader-program)
    (gl-bind-vertex-array gl-vao)
    ;; (gl-draw-elements GL-TRIANGLES 6 GL-UNSIGNED-INT 0)

    (map-indexed
     (lambda (cube i)
       (let ((model (rotate-matrix (translate-matrix (identity-matrix4)
						     cube)
				   (fl* (glfwGetTime)
					(degree->radian
					 (fl* 20.0 (fixnum->flonum i))))
				   (make-vector3 1.0 0.3 0.5))))
	 (set-uniform-matrix! model "model")
	 (gl-draw-arrays GL-TRIANGLES 0 36)))
     cube-positions)

    (glfwSwapBuffers window)
    (glfwPollEvents)
    state))

(define loop
  (lambda (state)
    (if (or (= 1 (glfwWindowShouldClose window))
	    (game-state-should-close? state))
	(glfwDestroyWindow window)
        (loop (draw-frame (process-input window state))))))


(glfwSetCursorPosCallback window
			  (lambda->ffi-callback mouse-callback
						(uptr double double)
						void))

(glfwSetScrollCallback window
		       (lambda->ffi-callback scroll-callback
					     (uptr double double)
					     void))


(loop (make-game-state (default-camera) (glfwGetTime) #f))

