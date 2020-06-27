
;; (define vertex-input? vertex-buffer-data?)

(define white-color (list 1.0 1.0 0.0))

(define model->vertex-input-metadata
  (case-lambda
   ((model-file)
    (apply model->vertex-input-metadata
	   (fold-left
	    (lambda (data-acc mesh-data)
	      (match-let* (((vertices0 normals0 uvs0 colors0 indices0 components0) data-acc)
			   (($ model-data vertex-data indices-data) mesh-data)
			   (($ vertex-buffer-data vertices normals uvs colors) vertex-data))
		(let ((mesh-index-count  (length indices-data))
		      (index-base (if (null? components0)
				      0
				      (fx+ (mesh-component-index-base (car components0))
					   (mesh-component-index-count (car components0))))))
		  (list (append vertices0 vertices)
			(append normals0 normals)
			(append uvs0 uvs)
			(append colors0 (or colors (list white-color)))
			(append indices0 indices-data)
			(cons (make-mesh-component mesh-index-count index-base)
			      components0)))))
	    '(() () () () () ())
	    (filter (lambda (m) (not (null? m))) (import-model model-file)))))
   ((vertices normals uvs colors indices-data components)
    (define sizeof-vertex-input
      (lambda ()
	(fx* 4
	     (length vertices)
	     (length normals)
	     ;; (length (or colors (list white-color)))
	     (length uvs))))

    (define vertex-input->list
      (lambda ()
	(displayln "length of vertices" (length vertices)
		   "colors" (length colors)
		   "uvs" (length uvs))
	(apply append (map append
			   vertices
			   normals
			   ;; todo colors not being captured
			   ;; (or  (map (lambda (_) white-color) vertices))
			   uvs))))

    (define vertex-input-stride
      (lambda ()
	(fx* 4
	     (+ (length (car vertices))
		(length (car normals))
		;; (length (or (and colors (car colors)) white-color))
		(length (car uvs))))))

    (make-vertex-input-metadata (sizeof-vertex-input)
				(vertex-input->list)
				(vertex-input-stride)
				indices-data
				(vector->attr (list (car vertices)
						    (car normals)
						    ;; (or (and colors (car colors)) white-color)
						    (car uvs)))
				(reverse components)))))

;; usage

;; (define input-metadata (model->vertex-input-metadata "models/box.obj"))

;; (length (vertex-input-metadata-vertices-list input-metadata))
