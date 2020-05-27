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

  (include "assimp.scm"))
