#! /usr/bin/scheme

(library-directories '("./thunderchez" "."))

;; (library (app)
;;   (export run))
(import (chezscheme)
	(vulkan)
	(matchable))

(define state (setup-vulkan))

;; (define run
;;   (lambda ()))
(let ((state (setup-vulkan))
      (shaders (make-shaders "shaders/shader.vert" "shaders/shader.frag")))
  (match-let* (((vertex-input-metadata . pipeline)
		(create-pipeline state
				 shaders
				 "models/sampleroom.dae"))
	       ((uniform-buffers . command-buffers) (create-buffers state
								    vertex-input-metadata
								    pipeline)))
    (run-draw-loop state uniform-buffers command-buffers)))

;; (parameterize ([optimize-level 3]
;; 	       [debug-level 0])
;;   (compile-library "app.scm"))
