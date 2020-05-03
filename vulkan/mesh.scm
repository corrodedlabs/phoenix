
(define vertex-input? vertex-buffer-data?)

(define white-color (list 1.0 1.0 0.0))

(define model->vertex-input-metadata
  (case-lambda
   ((model-file)
    (match (import-model model-file)
      (($ model-data vertex-data indices-data)
       (match vertex-data
	 (($ vertex-buffer-data vertices normals uvs colors)
	  (model->vertex-input-metadata vertices normals uvs colors indices-data))))))
   ((vertices normals uvs colors indices-data)
    (define sizeof-vertex-input
      (lambda ()
	(fx* 4
	     (length vertices)
	     ;; (length normals)
	     (length (or colors (list white-color)))
	     (length uvs))))

    (define vertex-input->list
      (lambda ()
	(apply append (map append
			   vertices
			   ;; normals
			   (or colors (map (lambda (_) white-color) vertices))
			   uvs))))

    (define vertex-input-stride
      (lambda ()
	(fx* 4
	     (+ (length (car vertices))
		;; (length (car normals))
		(length (or (and colors (car colors)) white-color))
		(length (car uvs))))))

    (make-vertex-input-metadata (sizeof-vertex-input)
				(vertex-input->list)
				(vertex-input-stride)
				indices-data
				(vector->attr (list (car vertices)
						    ;; (car normals)
						    (or (and colors (car colors)) white-color)
						    (car uvs)))))))

;; usage

;; (define input-metadata (create-vertex-input-metadata "models/box.obj"))

;; (length (vertex-input-metadata-vertices-list input-metadata))
