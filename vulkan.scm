(define vulkan (load-shared-object "libvulkan.so"))

(define-condition-type &ffi-condition &condition
  ffi-condition ffi-condition?
  (msg ffi-condition-msg)
  (ptr ffi-condition-ptr)
  (context ffi-condition-context))


(define-syntax make-foreign-object
  (syntax-rules ()
    ((_ struct)  (make-ftype-pointer struct (foreign-alloc (ftype-sizeof struct))))))

(define-syntax define-ptr-lambda 
  (syntax-rules ()
    ((_ lambda-name struct-name member-name f)
     (define lambda-name
       (lambda (obj)
	 (cond
	  ((ftype-pointer? struct-name obj)
	   (f struct-name (member-name) obj))
	  (else (raise (ffi-condition "invalid pointer" obj lambda-name)))))))))


(define-syntax define-foreign-struct
  (lambda (stx)

    (define construct-name
      (lambda (template-identifier . args)
	(datum->syntax template-identifier
	  (string->symbol (apply string-append (map (lambda (x)
						      (if (string? x)
							x
							(symbol->string (syntax->datum x))))
						    args))))))

    (define construct-make-def
      (lambda (struct-name member-spec)
	(with-syntax ([struct-name struct-name]
		      [((setters . val-exprs) ...)
		       (map (lambda (member)
			      (with-syntax ((name (car member))
					    (type (cdr member))
					    (struct-name struct-name))
				(with-syntax ([val-expr (construct-name #'struct-name
									"val-"
									#'name)])
				  (cons #'(ftype-set! struct-name (name) obj val-expr)
					#'val-expr))))
			    member-spec)])
	  #'(lambda (val-exprs ...)
	      (let ((obj (make-foreign-object struct-name)))
		(begin setters ...))))))

    (define construct-val-defs
      (lambda (struct-name member-names)
	(with-syntax ((struct-name struct-name))
	  (map (lambda (member-name)
		 (with-syntax ((lambda-name (construct-name #'struct-name
						       #'struct-name "-" member-name))
			       (lambda-&name (construct-name #'struct-name
							#'struct-name "-&" member-name))
			       (member-name member-name))
		   #'(begin (define-ptr-lambda lambda-name struct-name member-name ftype-ref)
			    (define-ptr-lambda lambda-&name struct-name member-name ftype-&ref))))
	       member-names))))
    
    (syntax-case stx ()
      [(_ struct-name ((member-name . member-type) ...))
       (for-all identifier? #'(struct-name member-name ...))
       (with-syntax ([struct-name #'struct-name]
		     [make-struct-name (construct-name #'struct-name "make-" #'struct-name)]
 		     [make-struct-def
		      (construct-make-def #'struct-name
		     			  #'((member-name . member-type) ...))]
		     [(val-defs ...)
		      (construct-val-defs #'struct-name #'(member-name ...))])
	 #'(begin
	     (define-ftype struct-name (struct [member-name member-type] ...))
	     (define make-struct-name make-struct-def)
	     val-defs ...))])))

(define-ftype vk-structure-type int)

(define-foreign-struct vk-application-info
  [(sType . vk-structure-type)
   (next . uptr)
   (application-name . (* char))
   (application-version . int)
   (engine-name . (* char))
   (engine-version . int)
   (api-version . int)])
