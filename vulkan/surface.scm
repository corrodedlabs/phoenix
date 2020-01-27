;; depends on
;; * glfw submodule https://github.com/chenguangqi/glfw

(define-record-type window-details (fields window surface))

(define-record-type dimension (fields width height))

(define create-surface
  (lambda (instance window)
    (let ((surface (make-foreign-object uptr)))
      (glfw-create-window-surface instance window 0 (ftype-pointer-address surface))
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

(glfw-init)

(define v (load-shared-object "libvulkan.so.1"))

(load "vulkan/enums.scm")
(load "vulkan/ftype.scm")
(load "vulkan/instance.scm")
(define ins (init-vulkan))

(load "vulkan/surface.scm")


(define window-obj (setup-window ins 1366 768))

(window-details? window-obj)
