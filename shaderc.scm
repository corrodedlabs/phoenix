(library (shaderc)

  (export shaderc-vertex-shader
	  shaderc-fragment-shader
	  shaderc-compute-shader
	  shaderc-geometry-shader
	  shaderc-tess-control-shader
	  shaderc-tess-evaluation-shader

	  compile-shaders)
  
  (import (chezscheme)
	  (ffi)
	  (only (srfi s13 strings) string-join))

  (define s (load-shared-object "libshaderc_shared.so"))

  ;;;;;;;;;;;;;;;;;;;;;;;
  ;; low level wrapper ;;
  ;;;;;;;;;;;;;;;;;;;;;;;

  ;; types

  (define-ftype shaderc-compiler uptr)
  (define-ftype shaderc-compile-options uptr)
  (define-ftype shaderc-compilation-result uptr)

  ;; functions

  (define _init-compiler
    (foreign-procedure "shaderc_compiler_initialize" () (* shaderc-compiler)))

  (define _init-compile-options
    (foreign-procedure "shaderc_compile_options_initialize" () (* shaderc-compile-options)))

  (define-enum-ftype shaderc-shader-kind
    shaderc-vertex-shader
    shaderc-fragment-shader
    shaderc-compute-shader
    shaderc-geometry-shader
    shaderc-tess-control-shader
    shaderc-tess-evaluation-shader)

  (define _compile->spv
    (foreign-procedure "shaderc_compile_into_spv"
		       ((* shaderc-compiler) string size_t shaderc-shader-kind string string
			(* shaderc-compile-options))
		       (* shaderc-compilation-result)))

  (define-enum-ftype shaderc-compilation-status
    shaderc-compilation-status-success
    shaderc-compilation-status-invalid-stage
    shaderc-compilation-status-compilation-error
    shaderc-compilation-status-internal-error
    shaderc-compilation-status-null-result-object
    shaderc-compilation-status-invalid-assembly
    shaderc-compilation-status-validation-error
    shaderc-compilation-status-configuration-error)

  (define _get-error-message
    (foreign-procedure "shaderc_result_get_error_message" ((* shaderc-compilation-result)) string))

  (define _get-num-warnings
    (foreign-procedure "shaderc_result_get_num_warnings" ((* shaderc-compilation-result)) size_t))

  (define _get-num-errors
    (foreign-procedure "shaderc_result_get_num_errors" ((* shaderc-compilation-result)) size_t))

  (define _get-compilation-status
    (foreign-procedure "shaderc_result_get_compilation_status"
		       ((* shaderc-compilation-result)) shaderc-compilation-status))

  (define _get-result-length
    (foreign-procedure "shaderc_result_get_length" ((* shaderc-compilation-result)) size_t))

  (define _get-result-bytes
    (foreign-procedure "shaderc_result_get_bytes"
		       ((* shaderc-compilation-result)) (* unsigned-32)))

  (define _release-result
    (foreign-procedure "shaderc_result_release" ((* shaderc-compilation-result)) void))

  (define _release-compiler
    (foreign-procedure "shaderc_compiler_release" ((* shaderc-compiler)) void))

  (define _release-compile-options
    (foreign-procedure "shaderc_compile_options_release" ((* shaderc-compile-options)) void))

  ;;;;;;;;;;;;;;;;;;;;
  ;; scheme wrapper ;;
  ;;;;;;;;;;;;;;;;;;;;

  (define file->string
    (lambda (filename)
      (call-with-input-file filename
	(lambda (p)
	  (let lp ((l (get-line p))
		   (res '()))
	    (cond
	     ((eof-object? l) (string-join (reverse res) "\n"))
	     (else (lp (get-line p)
		       (cons l res)))))))))

  ;; (file->string "shaders/shader.vert")

  (define compile-shaders
    (lambda (filename shader-kind)
      ;; todo perform cleanup
      (let* ((compiler (_init-compiler))
	     (options (_init-compile-options))
	     (glsl-code (file->string filename))
	     (result (_compile->spv compiler
				    glsl-code
				    (string-length glsl-code)
				    shader-kind
				    filename
				    "main"
				    options)))
	(cond
	 ((equal? (_get-compilation-status result)  0)
	  (make-array-pointer (_get-result-length result)
			      (_get-result-bytes result)
			      'unsigned-32))
	 (else (let ((num-errors (_get-num-errors result))
		     (num-warnings (_get-num-warnings result))
		     (error-message (_get-error-message result)))
		 (display "Num errors: ") (display num-errors) (newline)
		 (display "Num warnings: ") (display num-warnings) (newline)
		 (display "Error message: ") (display error-message) (newline)
		 (error "shader compilation failed: " (_get-compilation-status result)))))))))


;; (load "shaderc.scm")
;; (import (shaderc))
;; (define x (compile-shaders "shaders/shader.vert" shaderc-vertex-shader))
;; (compile-shaders "shaders/shader.frag" shaderc-fragment-shader)
