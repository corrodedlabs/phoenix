(define vulkan (load-shared-object "libvulkan.so"))

;; (define sample (load-shared-object "./sample.so"))

(define-condition-type &ffi-condition &condition
  ffi-condition ffi-condition?
  (msg ffi-condition-msg)
  (ptr ffi-condition-ptr)
  (context ffi-condition-context))

(define strdup (foreign-procedure "strdup" (string) (* char)))

(define-syntax write-cstring
  (syntax-rules ()
    ((_ ptr ftype field-accessor str)
     (ftype-set! ftype field-accessor ptr (strdup str)))))

(define ptr->string
  (lambda (ptr)
    (let loop ((i 0)
	       (chs '()))
      (let ((ch (foreign-ref 'char (ftype-pointer-address ptr) i)))
	 (cond
	  ((char=? ch #\nul) (list->string (reverse chs)))
	  (else (loop (+ i 1)
		      (cons ch chs))))))))


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
	   (let ((value (f struct-name (member-name) obj)))
	     (if (ftype-pointer? char value)
	       (ptr->string value)
	       value)))
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
				  (cons #'(cond
					   ((string? val-expr)
					    (write-cstring obj struct-name (name) val-expr))
					   (else (ftype-set! struct-name (name) obj val-expr)))
					#'val-expr))))
			    member-spec)])
	  #'(lambda (val-exprs ...)
	      (let ((obj (make-foreign-object struct-name)))
		setters ...
		obj)))))

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
(define-ftype cstring (* char))

(define-foreign-struct abc
  ((a . int)
   (d . cstring)))

(define-foreign-struct vk-application-info
  [(sType . vk-structure-type)
   (next . uptr)
   (application-name . cstring)
   (application-version . int)
   (engine-name . cstring)
   (engine-version . int)
   (api-version . int)])

;; (define a (make-abc 12 "world"))
;; (abc-&d a)

;; (read-cstring abc d a)

;; (fill-vulkan-struct vk-application-info
;; 		    ())

;; (define app-info (make-vk-application-info ))
