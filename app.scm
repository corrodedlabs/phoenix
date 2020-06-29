#! /usr/bin/scheme

(library-directories '("./thunderchez" "."))

;; (library (app)
;;   (export run))
(import (chezscheme)
	(vulkan)
	(matchable)
	(prelude))


;; (define run
;;   (lambda ()))
(let ((state (setup-vulkan))
      (shaders (make-shaders "shaders/shader.vert" "shaders/shader.frag")))
  (displayln "creating pipeline")
  (match-let* (((pipeline vertex-input-metadata render-pass)
		(create-pipeline state shaders "models/sampleroom.dae"))	       
	       ((command-buffers uniform-buffers camera-matrix)
		(create-buffers state pipeline vertex-input-metadata render-pass)))
    (run-draw-loop state uniform-buffers command-buffers camera-matrix)))

;; (parameterize ([optimize-level 3]
;; 	       [debug-level 0])
;;   (compile-library "app.scm"))
