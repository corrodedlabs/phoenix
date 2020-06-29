(library (glfw)
  (export glfw-init
	  glfw-get-required-instance-extensions
	  create-window
	  new-window
	  poll-events
	  get-movement-direction
	  movement-data
	  movement-data?
	  movement-data-forward
	  movement-data-back
	  movement-data-right
	  movement-data-left
	  load-glfw)

  (import (chezscheme)
	  (prelude)
	  (ffi)
	  (glfw glfw))

  (define load-glfw (lambda ()
		      (display "loading glfw") (newline)
		      (load-shared-library (make-library-detail #f
								"libglfw.so"
								"phoenix-libs/glfw3.dll"))))
  (define lib (load-glfw))

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

  ;; Input Mode

  ;; STICKY_KEYS
  ;; When sticky keys mode is enabled, the pollable state of a key will remain
  ;; GLFW_PRESS until the state of that key is polled with glfwGetKey. Once it has been
  ;; polled, if a key release event had been processed in the meantime, the state will reset
  ;; to GLFW_RELEASE, otherwise it will remain GLFW_PRESS.
  ;;
  ;; glfwSetInputMode(window, GLFW_STICKY_KEYS, GLFW_TRUE);

  ;; LOCK_KEY_MODS
  (define GLFW_LOCK_KEY_MODS  #x00033004)
  
  ;; If you wish to know what the state of the Caps Lock and Num Lock keys was when input
  ;; events were generated, set the GLFW_LOCK_KEY_MODS input mode.
  ;;
  ;; glfwSetInputMode(window, GLFW_LOCK_KEY_MODS, GLFW_TRUE);
  ;;
  ;; When this input mode is enabled, any callback that receives modifier bits will have the
  ;; GLFW_MOD_CAPS_LOCK bit set if Caps Lock was on when the event occurred and the
  ;; GLFW_MOD_NUM_LOCK bit set if Num Lock was on.

  ;; The GLFW_KEY_LAST constant holds the highest value of any named key.
  (define setup-input-mode
    (lambda (window)
      (glfwSetInputMode window GLFW_STICKY_KEYS GLFW_TRUE)
      (glfwSetInputMode window GLFW_LOCK_KEY_MODS GLFW_TRUE)))
  

  (define create-window
    (lambda (vk-instance width height)
      (let ((window (new-window width height)))
	(setup-input-mode window)
	(make-window window (create-surface vk-instance window)))))

  ;; GLFW needs to poll the window system for events both to provide input to the application
  ;; and to prove to the window system that the application hasn't locked up.
  ;; Event processing is normally done each frame after buffer swapping. Even when you have
  ;; no windows, event polling needs to be done in order to receive monitor and joystick
  ;; connection events.

  ;; glfwPollEvents();
  ;; This is the best choice when rendering continuously, like most games do.

  ;; There are three functions for processing pending events.
  ;; glfwPollEvents processes only those events that have already been received and then
  ;; returns immediately.
  (define poll-events glfwPollEvents)

  ;; get-keys
  ;;
  ;; This function returns the last state reported for the specified key to the specified
  ;; window. The returned state is one of GLFW_PRESS or GLFW_RELEASE. The higher-level action
  ;; GLFW_REPEAT is only reported to the key callback.

  ;; If the GLFW_STICKY_KEYS input mode is enabled, this function returns GLFW_PRESS the first
  ;; time you call it for a key that was pressed, even if that key has already been released.

  ;; The key functions deal with physical keys, with key tokens named after their use on the
  ;; standard US keyboard layout.
  ;; If you want to input text, use the Unicode character callback instead.

  ;; The modifier key bit masks are not key tokens and cannot be used with this function.

  ;; Do not use this function to implement text input.

  ;; Parameters
  ;; [in]	window	The desired window.
  ;; [in]	key	The desired keyboard key. GLFW_KEY_UNKNOWN is not a valid key for
  ;; this function.
  ;; Returns
  ;; One of GLFW_PRESS or GLFW_RELEASE.
  ;; Errors
  ;; Possible errors include GLFW_NOT_INITIALIZED and GLFW_INVALID_ENUM.
  ;; Thread safety
  ;; This function must only be called from the main thread.
  ;; See also
  ;; Key input
  ;; Since
  ;; Added in version 1.0. GLFW 3: Added window handle parameter.
  (define get-key glfwGetKey)

  (define key-pressed?
    (lambda (window key)
      (equal? (get-key window key) GLFW_PRESS)))

  ;; record to capture the direction of movement
  ;; all the fields are boolean
  ;; this record will be enriched as and when we need to capture different commands
  (define-record-type movement-data (fields forward back right left))

  (define get-movement-direction
    (lambda (window)
      (let ((forward (key-pressed? window GLFW_KEY_W))
	    (back    (key-pressed? window GLFW_KEY_S))
	    (right   (key-pressed? window GLFW_KEY_D))
	    (left    (key-pressed? window GLFW_KEY_A)))
	(make-movement-data forward back right left))))

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
