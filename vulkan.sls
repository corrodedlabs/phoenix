#!chezscheme
(library (vulkan)

  (export setup-vulkan
	  make-shaders
	  create-pipeline
	  create-buffers
	  run-draw-loop)

  (import (chezscheme)
	  (prelude)
	  (ffi)
	  (glfw)
	  (assimp)
	  (glfw)
	  (shaderc)
	  (camera)
	  (image)
	  (matchable)
	  
	  (vulkan structure-types))

  (define g (load-glfw))
  (define v (load-shared-library (make-library-detail #f 
						      "libvulkan.so.1"
						      "vulkan-1.dll")))

  (include "vulkan/enums.scm")
  (include "vulkan/ftype.scm")

  (include "vulkan/instance.scm")
  (include "vulkan/surface.scm")

  (include "vulkan/queues.scm")
  (include "vulkan/devices.scm")
  (include "vulkan/swapchain.scm")

  (include "vulkan/images.scm")
  (include "vulkan/pipeline.scm")
  (include "vulkan/mesh.scm")
  (include "vulkan/texture.scm")
  (include "vulkan/buffers.scm")

  (include "vulkan/sync.scm")
  (include "vulkan.scm"))
