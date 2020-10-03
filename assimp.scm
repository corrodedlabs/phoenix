;;
;; ffi bindings for assimp
;;

;; Meshes

;; some constants

;; ---------------------------------------------------------------------------
;; Limits. These values are required to match the settings Assimp was
;; compiled against. Therefore, do not redefine them unless you build the
;; library from source using the same definitions.
;; ---------------------------------------------------------------------------

;; Maximum number of indices per face (polygon).
(define %max-face-indices% #x7fff)

;; Maximum number of indices per face (polygon).
(define %max-bone-weights% #x7fffffff)

;; Maximum number of vertices per mesh.
(define %max-vertices% #x7fffffff)

;; Maximum number of faces per mesh.
(define %max-faces% #x7fffffff)

;; Supported number of vertex color sets per mesh.
(define %max-number-of-color-sets% #x8)

;; Supported number of texture coord sets (UV(W) channels) per mesh
(define %max-number-of-texture-coords% #x8)

;; structs

(define-ftype ai-real float)

(define-collection-lambdas unsigned)

(define-foreign-struct vector3
  ((x . ai-real)
   (y . ai-real)
   (z . ai-real)))

;; defines collection fns like
;; vector3-pointer-map to work on the array pointers
(define-collection-lambdas vector3)

;; other useful utilities for working with vector3 ptrs

(define vector3-ptr->list
  (lambda (ptr)
    (list (vector3-x ptr) (vector3-y ptr) (vector3-z ptr))))


(define vector3-array-ptr->list
  (lambda (array-ptr)
    (and (array-pointer? array-ptr) (vector3-pointer-map vector3-ptr->list array-ptr))))

;; matrix
;; // ---------------------------------------------------------------------------
;; /** @brief Represents a row-major 4x4 matrix, use this for homogeneous
;; *   coordinates.
;; *
;; *  There's much confusion about matrix layouts (column vs. row order).
;; *  This is *always* a row-major matrix. Not even with the
;; *  #aiProcess_ConvertToLeftHanded flag, which absolutely does not affect
;; *  matrix order - it just affects the handedness of the coordinate system
;; *  defined thereby.
;; */
(define-foreign-struct ai-matrix-4x4
  ((a1 . ai-real) (a2 . ai-real) (a3 . ai-real) (a4 . ai-real)
   (b1 . ai-real) (b2 . ai-real) (b3 . ai-real) (b4 . ai-real)
   (c1 . ai-real) (c2 . ai-real) (c3 . ai-real) (c4 . ai-real)
   (d1 . ai-real) (d2 . ai-real) (d3 . ai-real) (d4 . ai-real)))


;; string

;; Maximum dimension for strings, ASSIMP strings are zero terminated.
(define +maxlen+ 1024)

;; Represents an UTF-8 string, zero byte terminated.
;; The (binary) length of such a string is limited to MAXLEN characters
;; (including the terminating zero).
(define-foreign-struct ai-string
  ((length . unsigned-32)
   (data . (array 1024 char))))

;; colors

(define-foreign-struct color4
  ((r . ai-real)
   (g . ai-real)
   (b . ai-real)
   (a . ai-real)))

;; A single face in a mesh, referring to multiple vertices.
;; If mNumIndices is 3, we call the face 'triangle', for mNumIndices > 3
;; it's called 'polygon' (hey, that's just a definition!).

(define-foreign-struct face
  ((num-indices . unsigned)
   (indices . (* unsigned))))

(define-collection-lambdas face)

(define-ftype aabb
  (struct (min vector3)
	  (max vector3)))

;; // -------------------------------------------------------------------------------
;; /** 
;; * A node in the imported hierarchy.
;; *
;; * Each node has name, a parent node (except for the root node),
;; * a transformation relative to its parent and possibly several child nodes.
;; * Simple file formats don't support hierarchical structures - for these formats
;; * the imported scene does consist of only a single root node without children.
;; */
;; // -------------------------------------------------------------------------------
(define-foreign-struct node
  ((name . ai-string)
   (transformation . ai-matrix-4x4)
   (parent . (* node))
   (num-children . unsigned)
   (children . uptr)
   (num-meshes . unsigned)
   (meshes . (* unsigned))
   (metadata . uptr)))

(define-collection-lambdas node)

;; struct representing a geometry or model with a single material
;; A mesh uses only a single material which is referenced by a material ID
(define-foreign-struct mesh
  ((primitive-types .  unsigned)
   (num-vertices . unsigned)
   (num-faces . unsigned)
   (vertices . (* vector3))
   (normals  . (* vector3))
   (tangents . (* vector3))
   (bit-tangents . (* vector3))
   (colors . (array #x8 (* color4)))
   (texture-coords . (array #x8 (* vector3)))
   (num-uv-component  . (array #x8 unsigned))
   (faces . (* face))
   (num-bones . unsigned)
   (bones . uptr)
   (material-index . unsigned)
   (name . ai-string)
   (num-anim-meshes . unsigned)
   (anim-meshes . uptr)
   (method . unsigned)))

(define-collection-lambdas mesh)

(define-foreign-struct scene
  ((flags . unsigned)
   (root-node . (* node))
   (num-meshes . unsigned)
   (meshes . uptr)
   (num-materials .  unsigned)
   (materials .  uptr)
   (num-animations .  unsigned)
   (animations . uptr)
   (num-textures . unsigned)
   (textures . uptr)
   (num-lights . unsigned)
   (lights . uptr)
   (num-cameras .  unsigned)
   (cameras . uptr)
   (metadata . uptr)))

(define-enum-ftype ai-process-steps
  (calc-tangent-space #x1)
  (join-identical-vertices #x2)
  (make-left-handed #x4)
  (triangulate #x8)
  (remove-component #x10)
  (gen-normals #x20)
  (gen-smooth-normals #x40)
  (split-large-meshes #x80)
  (pretransform-vertices #x100)
  (limit-bone-weights #x200)
  (validate-data-structure #x400)
  (improve-cache-locality #x800)
  (remove-redundant-materials #x1000)
  (fix-infacing-normals #x2000)
  (sort-by-ptype #x8000)
  (find-degenerates #x10000)
  (find-invalid-data #x20000)
  (gen-UV-coords #x40000)
  (transform-UV-coords #x80000)
  (find-instances #x100000)
  (optimize-meshes #x200000)
  (optimize-graph #x400000)
  (flip-UVs #x800000)
  (flip-winding-order #x1000000)
  (split-by-bone-count #x2000000)
  (debone #x4000000))


;; load library

(define o (load-shared-object "libassimp.so"))

;; define native functions

(define import_file (foreign-procedure "aiImportFile" (string unsigned) (* scene)))
(define get_error_string (foreign-procedure "aiGetErrorString" () string))

;; run

;; Vulkan uses a right-handed NDC (contrary to OpenGL), so simply flip Y-Axis
(define flip-vertices
  (lambda (vec3-list)
    (list (car vec3-list) (* -1.0 (cadr vec3-list)) (caddr vec3-list))))


;; record to collect vertex buffer data
(define-record-type vertex-buffer-data
  (nongenerative)
  (fields vertices normals uv colors))

(define (mesh-ptr->vertex-buffer-data mesh-ptr)
  (let* ((num-vertices (mesh-num-vertices mesh-ptr))

	 (vertices
	  (map flip-vertices
	       (vector3-array-ptr->list (make-array-pointer num-vertices
							    (mesh-vertices mesh-ptr)
							    'vector3))))
	 (normals
	  (vector3-array-ptr->list (make-array-pointer num-vertices
						       (mesh-normals mesh-ptr)
						       'vector3)))

	 ;; Texture coordinates may have multiple channels, we only use the first we find
	 (texture-coords-channel
	  (find (lambda (ptr)
		  (not (ftype-pointer-null? ptr))) (mesh-texture-coords mesh-ptr)))
	 
	 (texture-coords
	  (map (lambda (uv-coords)
		 (cond
		  ((list? uv-coords) (list (car uv-coords) (cadr uv-coords)))
		  (else (error "uv channels are missing"  mesh-ptr))))
	       (vector3-array-ptr->list (make-array-pointer num-vertices
							    texture-coords-channel
							    'vector3)))))
    (displayln "vertices are number: " num-vertices)
    (make-vertex-buffer-data vertices normals texture-coords #f)))

(define concatenate (lambda (xs) (apply append xs)))

;; data for index buffer
(define mesh-ptr->indices
  (lambda (mesh-ptr vertex-offset)     
    (displayln "num of faces is " (mesh-num-faces mesh-ptr))
    (concatenate (face-pointer-map
		  (lambda (face-ptr)
		    (unsigned-pointer-map (lambda (x) (+ vertex-offset (read-unsigned x)))
					  (make-array-pointer (face-num-indices face-ptr)
							      (face-indices face-ptr)
							      'unsigned)))
		  (make-array-pointer (mesh-num-faces mesh-ptr)
				      (mesh-faces mesh-ptr)
				      'face)))))


;; record to collect the data required from assimp
(define-record-type model-data
  (nongenerative)
  ;; vertex-data: contains data recorded per vertex type record<vertex-buffer-data>
  ;; indices: list containing indices from the mesh
  (fields vertex-data indices))



;; (define model "models/Sponza-master/sponza.obj")


(define x (list))


;; fn to import a model
;; todo improve error handling in case file is not found
(define import-model
  (lambda (model)

    (define mesh-ptr->model-data
      (lambda (mesh-ptr vertex-offset)
	(let ((vertex-data (mesh-ptr->vertex-buffer-data mesh-ptr))
	      (indices (mesh-ptr->indices mesh-ptr vertex-offset)))
	  (make-model-data vertex-data indices))))

    (define mesh-indices->model-data
      (lambda (mesh-indices mesh-list vertex-offset)
	(unsigned-pointer-map (lambda (mesh-index)
				(mesh-ptr->model-data (list-ref mesh-list
								(read-unsigned mesh-index))
						      vertex-offset))
			      mesh-indices)))

    (define process-node
      (lambda (node-ptr meshes-list collected-mesh-data)
	(let* ((num-meshes (node-num-meshes node-ptr))
	       (mesh-indices (make-array-pointer num-meshes (node-meshes node-ptr) 'unsigned))
	       (num-children (node-num-children node-ptr))
	       (child-nodes (double-pointer->list (node-children node-ptr)
						  node
						  num-children))
	       (vertex-offset (fold-left (lambda (offset model-data)
					   (+ offset
					      (length (vertex-buffer-data-vertices
						       (model-data-vertex-data model-data)))))
					 0
					 collected-mesh-data)))
	  (let ((current-mesh-data (mesh-indices->model-data mesh-indices
							     meshes-list
							     vertex-offset)))
	    (fold-left (lambda (data child-node-ptr)
			 (process-node child-node-ptr meshes-list data))
		       (append collected-mesh-data current-mesh-data)
		       child-nodes)))))
    
    (let* ((scene-ptr (import_file model
				   (bitwise-ior pretransform-vertices
						triangulate
						gen-normals)))
	   (meshes-list (double-pointer->list (scene-meshes scene-ptr)
					      mesh
					      (scene-num-meshes scene-ptr))))
      (displayln "scene meshes " (scene-num-meshes scene-ptr))
      (process-node (scene-root-node scene-ptr) meshes-list (list)))))

;; Example usage:

;; > (load "assimp.scm")
;; > (import (assimp))
(define model-file "models/sampleroom.dae")
(define model-data-obj (import-model model-file))
;; (define model-data-obj (import-model "models/cube.obj"))
;; (define model-data-obj (import-model "models/Sponza-master/sponza.obj"))
;; (length (vertex-buffer-data-vertices (model-data-vertex-data model-data-obj)))
