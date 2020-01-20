;; (load "vulkan/instance.scm")

(import (vulkan instance))

(define main
  (lambda ()
    (define instance (make-vulkan-instance #t))
    (define messenger (create-debug-utils-messenger instance))
    (get-physical-devices instance)))
