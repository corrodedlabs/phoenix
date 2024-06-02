#! /usr/bin/scheme

(library-directories '("./thunderchez" "."))

(import (chezscheme)
	(vulkan)
	(matchable))

;; (define (print-stack-trace e)

;;   (define (get-func c)
;;     (let ((cc ((c 'code) 'name)))
;;       (if cc cc "--main--")))

;;   (display-condition e) (newline)

;;   (let p ((t (inspect/object (condition-continuation e))))
;;     (call/cc
;;      (lambda (ret)
;;        (if (> (t 'depth) 1)
;;            (begin
;; 	     (call-with-values
;; 		 (lambda () (t 'source-path))
;; 	       (case-lambda
;; 		((file line column)
;; 		 (printf "\tat ~a (~a:~a,~a)\n" (get-func t) file line column))
;; 		(else (ret))))
;; 	     (p (t 'link)))))))
;;   (exit))

;; (base-exception-handler print-stack-trace)


(define state (setup-vulkan))

(define shaders (make-shaders "shaders/shader.vert" "shaders/shader.frag"))

(define p (create-pipeline-from-model state
				      shaders
				      "models/cube.obj"))

(define vertex-input-metadata (car p))
(define pipeline (cdr p))

(define buffers (create-buffers state
				vertex-input-metadata
				pipeline))


(define uniform-buffers (car buffers))
(define command-buffers (cdr buffers))

(run-draw-loop state uniform-buffers command-buffers)

;; (let ((state (setup-vulkan))
;;       (shaders ))
;;   (match-let* (((vertex-input-metadata . pipeline)
;; 		)
;; 	       ((uniform-buffers . command-buffers) ))
;;     ))

;; (parameterize ([optimize-level 3]
;; 	       [debug-level 0])
;;   (compile-library "app.scm"))
