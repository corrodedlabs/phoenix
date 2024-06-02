(library (math)
  (export make-vector3
	  make-vector4
	  constant-vector3
	  constant-vector4

	  identity-matrix4
	  matrix->list

	  x-axis
	  y-axis
	  z-axis
	  degree->radian
	  rotate-matrix
	  scale-matrix
	  translate-matrix
	  opengl-perspective-matrix
	  look-at
	  vector3-
	  vector3?
	  normalize-vector
	  vector-cross
	  vector-dot

	  matrix4*
	  vector3+
	  scale-vector)
  (import (except (scheme) vector->list)
	  (matchable)
	  (prelude))

  (include "math/vector.scm")
  (include "math/matrix.scm")
  (include "math/transformations.scm"))
