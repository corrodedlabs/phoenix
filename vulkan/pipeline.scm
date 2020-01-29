;; depends on shaderc library
;; defined in shaderc.scm at the root


(define create-shader-stages
  (lambda (device vertex-shader-filename fragment-shader-filename)
    
    (define create-shader-module
      (lambda (spv-array-pointer)
	(let ((info (make-vk-shader-module-create-info shader-module-create-info 0 0
						       (array-pointer-length spv-array-pointer)
						       (array-pointer-raw-ptr spv-array-pointer)))
	      (module (make-foreign-object vk-shader-module)))
	  (vk-create-shader-module device info 0 module)
	  module)))

    (define create-shader-stage-info
      (lambda (shader-stage-bit module)
	(make-vk-pipeline-shader-stage-create-info pipeline-shader-stage-create-info 0 0
						   shader-stage-bit
						   (pointer-ref-value module)
						   "main"
						   0)))
    
    (let ((vertex-spv (compile-shaders vertex-shader-filename shaderc-vertex-shader))
	  (frag-spv (compile-shaders fragment-shader-filename shaderc-fragment-shader)))
      (list (create-shader-stage-info vk-shader-stage-vertex-bit
				      (create-shader-module vertex-spv))
	    (create-shader-stage-info vk-shader-stage-fragment-bit
				      (create-shader-module frag-spv))))))


#!eof

(create-shader-stages (vulkan-state-device vs)
		      "shaders/shader.vert"
		      "shaders/shader.frag")
