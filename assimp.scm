(library (assimp)
  (export import-model
	  
	  vertex-buffer-data?
	  vertex-buffer-data-vertices
	  vertex-buffer-data-normals
	  vertex-buffer-data-uv
	  vertex-buffer-data-colors

	  model-data?
	  model-data-vertex-data
	  model-data-indices)
  (import (chezscheme)
	  (ffi)
	  (prelude))
  ;; test
  ;; ffi bindings for assimp

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

  (define-ftype ai-real double-float)

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
     (name . uptr)
     (num-anim-meshes . unsigned)
     (anim-meshes . uptr)
     (method . unsigned)))

  (define-foreign-struct scene
    ((flags . unsigned-32)
     (root-node . uptr)
     (num-meshes . unsigned-32)
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

  (define import_file
    (foreign-procedure "aiImportFile" (string unsigned) (* scene)))


  ;; run

  ;; Vulkan uses a right-handed NDC (contrary to OpenGL), so simply flip Y-Axis
  (define flip-vertices
    (lambda (vec3-list)
      (list (car vec3-list) (* -1.0 (cadr vec3-list)) (caddr vec3-list))))


  ;; record to collect vertex buffer data
  (define-record-type vertex-buffer-data (fields vertices normals uv colors))

  (define (mesh-ptr->vertex-buffer-data mesh-ptr)
    
    (define num-vertices (mesh-num-vertices mesh-ptr))

    (define vertices
      (map flip-vertices
	   (vector3-array-ptr->list (make-array-pointer num-vertices
							(mesh-vertices mesh-ptr)
							'vector3))))


    (define normals
      (vector3-array-ptr->list (make-array-pointer num-vertices (mesh-normals mesh-ptr) 'vector3)))


    ;; Texture coordinates may have multiple channels, we only use the first we find
    (define texture-coords-channel
      (find (lambda (ptr)
	      (not (ftype-pointer-null? ptr))) (mesh-texture-coords mesh-ptr)))

    (define texture-coords
      (vector3-array-ptr->list (make-array-pointer num-vertices texture-coords-channel 'vector3)))

    (make-vertex-buffer-data vertices normals texture-coords #f))


  ;; data for index buffer
  (define mesh-ptr->indices
    (lambda (mesh-ptr)
      
      (define concatenate (lambda (xs) (apply append xs)))
      
      (concatenate (face-pointer-map
		    (lambda (face-ptr)
		      (unsigned-pointer-map (lambda (ptr) (read-unsigned ptr))
					    (make-array-pointer (face-num-indices face-ptr)
								(face-indices face-ptr)
								'unsigned)))
		    (make-array-pointer (mesh-num-faces mesh-ptr)
					(mesh-faces mesh-ptr)
					'face)))))


  ;; record to collect the complete data required from assimp
  (define-record-type model-data
    ;; vertex-data: contains data recorded per vertex type record<vertex-buffer-data>
    ;; indices: list containing indices from the mesh
    (fields vertex-data indices))

  ;; fn to import a model
  (define import-model
    (lambda (model)
      (define scene-ptr
	(import_file model
		     (bitwise-ior flip-winding-order triangulate pretransform-vertices)))


      ;; just reading the first mesh
      (define mesh-ptr
	(make-ftype-pointer mesh (foreign-ref 'uptr (scene-meshes scene-ptr) 0)))

      (let ((vertex-data (mesh-ptr->vertex-buffer-data mesh-ptr))
	    (indices (mesh-ptr->indices mesh-ptr)))
	(make-model-data vertex-data indices)))))

;; Example usage:

;; > (load "assimp.scm")
;; > (import (assimp))

;; > (define model-data-obj (import-model "models/turret.obj"))

