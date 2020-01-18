;; (load "vulkan/instance.scm")

(import (vulkan instance))

(define main
  (lambda ()
    (define instance (make-vulkan-instance #t))
    (create-debug-utils-messenger instance)))
