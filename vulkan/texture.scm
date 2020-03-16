
(define texture-path "textures/winter.jpeg")

(define texture-image
  (create-texture-image physical-device device command-pool graphics-queue swapchain texture-path))
