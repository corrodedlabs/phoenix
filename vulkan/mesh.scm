

(define model-data-obj (import-model "models/turret.obj"))

(define vertex-data (model-data-vertex-data model-data-obj))


(define vertex-input? vertex-buffer-data?)

(define create-vertex-input-metadata
  (case-lambda
   ((model-file)
    (match (import-model model-file)
      (($ model-data vertex-data indices-data)
       (match vertex-data
	 (($ vertex-buffer-data vertices normals uvs colors)
	  (create-vertex-input-metadata vertices normals uvs colors indices-data))))))
   ((vertices normals uvs colors indices-data)
    (define sizeof-vertex-input
      (lambda ()
	(fx* 4 (length vertices) (length normals) (length uvs) (length (or colors '())))))

    (define vertex-input->vector
      (lambda ()
	(list->vector (append vertices normals uvs (or colors '())))))

    (define vertex-input-stride
      (lambda ()
	(fx* 4
	     (+ (length (car vertices))
		(length (car normals))
		(length (car uvs))
		(length (or (and colors (car colors)) '()))))))

    (make-vertex-input-metadata (sizeof-vertex-input)
				(vertex-input->vector)
				(vertex-input-stride)
				indices-data
				(vector->attr
				 (filter identity (list (car vertices)
							(car normals)
							(car uvs)
							(and colors (car colors)))))))))

;; usage

(define input-metadata (create-vertex-input-metadata "models/turret.obj"))

