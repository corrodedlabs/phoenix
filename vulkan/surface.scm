;; depends on
;; * glfw submodule https://github.com/chenguangqi/glfw

(define-record-type window-details (fields window surface))

(define-ftype vk-surface (struct (surface uptr)))

;; assumes libglfw is loaded
(define-vulkan-command glfwCreateWindowSurface
  ((& vk-instance) uptr uptr (* vk-surface)) int)

(define create-surface
  (lambda (instance window)
    (let ((surface (make-foreign-object vk-surface)))
      (glfw-create-window-surface instance window 0 surface)
      surface)))

;; creates a new window and returns a window-details record wrapping surface and window
(define setup-window
  (lambda (instance width height)
    (let ((window (create-window width height)))
      (make-window-details window (create-surface instance window)))))


#!eof

--------------------------------------------

experiments:

(library-directories '("./thunderchez" "."))

(import (ffi)
	(glfw)
	(vulkan structure-types))

(define v (load-shared-object "libvulkan.so.1"))

(load "vulkan/ftype.scm")
(load "vulkan/instance.scm")
(define instance (init-vulkan))

(load "vulkan/surface.scm")


(define window-obj (setup-window instance 1366 768))
