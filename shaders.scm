(import (match match)
	(prelude))

;; implemented using list transformations

(define transform-var-name
  (lambda (var)
    (string->symbol (kebab-case->camel-case (symbol->string var)))))

(define expanded-lang->glsl
  (lambda (sexps)
    (let lp ((forms sexps)
	     (out '()))
      (cond
       ((null? forms)
	(apply string-append
	       (apply append
		      (map (lambda (stmt) (list stmt "\n")) (reverse out)))))

       (else
	(let ((form (car forms)))
	  (match form
	    ((define-version ,num)
	     (lp (cdr forms)
		 (cons (format "#version ~s" num) out)))

	    ((define-version ,num ,mod)
	     (lp (cdr forms)
		 (cons (format "#version ~s ~s" num mod) out)))

	    ((define-layout ,index ,meta-type ,type ,name)
	     (lp (cdr forms)
		 (cons (format "layout (location = ~s) ~s ~s ~s;"
			       index meta-type type name)
		       out)))

	    ((define-uniform sampler-2d ,name)
	     (lp (cdr forms)
		 (cons (format "uniform sampler2D ~s;"
			       (transform-var-name name)) out)))

	    ((,define-var ,type ,name)
	     (guard (member define-var '(define-in define-out)))
	     (lp (cdr forms)
		 (cons (format "out ~s ~s;" type name) out)))

	    ;; (define-lambda ret-type name <args> <body>)
	    ;; =>
	    ;; ret-type name (<args>)
	    ;; {
	    ;;   expand <body ...>
	    ;; }
	    ((define-lambda ,return-type ,name ,args ,body ...)
	     (let ((function-expansion (lp body '())))
	       (lp (cdr forms)
		   (cons (format "~s ~s~s \n { \n ~a \n }"
				 return-type
				 name
				 args
				 function-expansion)
			 out))))

	    ((set! ,var (,function ,args ...))
	     (let ((args
		    (interpose ","
			       (map (lambda (a)
				      (format "~s"
					      (if (symbol? a)
						  (transform-var-name a) a)))
				    args))))
	       (lp (cdr forms)
		   (cons (format "~s = ~s~a;"
				 (transform-var-name var)
				 function
				 args) out))))

	    ((set! ,var ,sym)
	     (lp (cdr forms) (cons (format "~s = ~s;" var sym) out)))

	    ((begin ,body ...) (lp body out))

	    (else (error 'expanding-vertex "unknown form" (car form))))))))))

(define layout->expanded-layout
  (lambda (layout-form)
    (mapcat (lambda (f)
	      (let ((dir-type (car f))
		    (vars (cdr f)))
		(map-indexed
		 (lambda (var i)
		   `(define-layout ,i ,dir-type ,(car var) ,(cadr var)))
		 vars)))
	    layout-form)))

(define out->expanded-out (lambda (out-form) (map identity out-form)))

(define lang->expanded-lang
  (lambda (exps)
    (let lp ((e exps) (out '()))
      (cond
       ((null? e) (reverse out))

       (else
	(let ((form (car e)))
	  (match form
	    ((define-layout (,layout-info ...))
	     (lp (cdr e)
		 (append (reverse (layout->expanded-layout layout-info))
			 out)))

	    ((,define-var (,out-vars ...))
	     (guard (member define-var '(define-in define-out)))
	     (lp (cdr e)
		 (append (reverse
			  (map (lambda (vars)
				 `(,define-var
				    ,@(map transform-var-name vars)))
			       out-vars)) out)))

	    ((define-uniform-block ,id (,args ...))
	     (lp (cdr forms)
		 (cons )))

	    ((define ,name (lambda (,args ...) ,body ...))
	     (let ((body-with-out
		    (reverse (cons 'set-out-position! (car (reverse body))))))
	       (lp (cdr e)
		   (cons `(define-lambda void ,name ,args
			    (begin ,@(lang->expanded-lang body-with-out))) out))))

	    ((set-out-position! ,x)
	     (lp (cdr e) (cons `(set! glPosition ,x) out)))

	    (,f (lp (cdr e) (cons form out))))))))))

(define-syntax expand-shader
  (lambda (stx)
    (define sexp->glsl
      (lambda (sexp)
	(expanded-lang->glsl (lang->expanded-lang (map syntax->datum sexp)))))

    (syntax-case stx ()
      ((_ exps ...)
       (with-syntax ((glsl (sexp->glsl (syntax->list #'(exps ...)))))
	 #'glsl)))))

(define-syntax define-vertex-shader
  (syntax-rules ()
    ((_ name body ...)
     (define name (expand-shader body ...)))))



;; tests and usage


(define sexps '((define-version 330 core)
		(define-layout 0 in vec3 pos)
		(define-lambda void main ()
		  (set! gl_Position (vec4 pos 1.0)))))

(display (expanded-lang->glsl sexps))

(define sexps
  '((define-version 330 core)
    (define-out vec4 fragVolor)
    (define-lambda void main ()
      (set! fragColor (vec4 1.0 0.5 0.2 1.0)))))
(display (expanded-lang->glsl sexps))


(define vertex
  '((define-version 330 core)
    (define-layout ((in vec3 pos)
		    (in vec3 color)
		    (in vec2 tex-coord)))
    (define-out ((vec3 out-color)
		 (vec2 tex-coord)))
    (define main
      (lambda ()
	(set-out-position! (vec4 pos 1.0))
	(set! out-color color)
	(set! tex-coord tex-coord)))))

(define fragment
  '((define-version 330 core)
    (define-out ((vec4 frag-color)))
    (define-in ((vec3 color) (vec2 tex-coord)))
    (define-uniform sampler-2d tex-sampler)
    (define main
      (lambda ()
	(set! frag-color (texture tex-sampler tex-coord))))))

(display (expanded-lang->glsl (lang->expanded-lang vertex))) (newline)
(display (expanded-lang->glsl (lang->expanded-lang fragment))) (newline)



(define-vertex-shader vertex
  (define-version 330 core)

  (define-layout ((in vec3 pos)
		  (in vec3 color)
		  (in vec2 tex-coord)))

  (define-out ((vec3 out-color)
	       (vec2 tex-coord)))

  (define main
    (lambda ()
      (set-out-position! (vec4 pos 1.0))
      (set! out-color color)
      (set! tex-coord tex-coord))))

(displayln vertex)


(define vertex
  '((define-version 450)
    ;; (enable-extension gl-arb-separate-shader-objects)

    (define-uniform-block ubo ((mat4 model) (mat4 view) (mat4 proj)))

    (define-layout ((in (vec3 pos)
			(vec3 color)
			(vec2 tex-coord))
		    (out (vec3 color)
			 (vec2 tex-coord))))

    (define main
      (lambda ()
	(* (model ubo) (view ubo) (proj ubo) (vec4 pos 1.0))))))

(define fragment
  '((define-version 450)
    (enable-extension gl-arb-separate-shader-objects)

    (define-uniform sampler-2d tex-sampler)

    (define-layout ((in (vec3 frag-color)
			(vec2 tex-coord))
		    (out (vec4 color))))

    (define main
      (lambda ()
	(* (texture tex-sampler tex-coord) (vec4 frag-color 1.0))))))

(display (expanded-lang->glsl (lang->expanded-lang vertex))) (newline)

#|

(load "shaders.scm")

|#
