(library (ffi)
  
  (export write-cstring
	  ptr->string
	  string->ptr
	  strdup
	  ptr->strings
	  strings->c-array
	  strings->ptr
	  
	  read-int
	  read-unsigned-32

	  memcpy
	  malloc
	  make-foreign-object
	  make-foreign-array
	  define-foreign-struct
	  define-enum-ftype
	  cstring
	  construct-name
	  null-pointer

	  define-collection-lambdas
	  make-array-pointer
	  array-pointer-length
	  array-pointer-raw-ptr
	  array-pointer-empty?
	  array-pointer?
	  with-new-array-pointer
	  pointer-ref-value)

  (import (chezscheme)
	  (prelude))

  (define libc (load-shared-object "libc.so.6"))

  (define memcpy
    (foreign-procedure "memcpy" (uptr uptr size_t) void))
  
  (define-condition-type &ffi-condition &condition
    ffi-condition ffi-condition?
    (msg ffi-condition-msg)
    (ptr ffi-condition-ptr)
    (context ffi-condition-context))

  (define (do-malloc size)
    (box (foreign-alloc size)))

  (define (do-free x)
    (foreign-free (unbox x)))

  (define malloc-guardian (make-guardian))

  (define malloc
    (lambda (size)
      (let ((x (do-malloc size)))
	(malloc-guardian x)
	x)))


  
  (define-ftype cstring (* char)) 

  (define ptr->string
    (lambda (ptr)
      (let loop ((i 0)
		 (chs '()))
	(let ((ch (foreign-ref 'char ptr i)))
	  (cond
	   ((char=? ch #\nul) (list->string (reverse chs)))
	   (else (loop (+ i 1)
		       (cons ch chs))))))))

  (define string->ptr
    (case-lambda
     [(str)
      (string->ptr str (unbox (malloc (* (ftype-sizeof char)
					 (string-length str)))) 0)]
     [(str ptr)
      (string->ptr str ptr 0)]
     [(str ptr offset)
      (let lp ((i offset))
	(cond
	 ((= i (+ (string-length str) offset))
	  (foreign-set! 'char ptr i #\nul)
	  ptr)
	 (else (foreign-set! 'char ptr i (string-ref str (- i offset)))
	       (lp (+ 1 i)))))]))

  ;; (string->ptr "abcd")


  (define strings->ptr
    (lambda (strs)
      (let ((ptr (make-ftype-pointer uptr
				     (unbox
				      (malloc (* (ftype-sizeof uptr)
						 (length strs)))))))
	(let lp ((i 0)
		 (s strs))
	  (cond
	   ((null? s) ptr)
	   (else (ftype-set! uptr
			     ()
			     ptr
			     i
			     (string->ptr (car s)))
		 (lp (+ 1 i) (cdr s))))))))

  (define strings->c-array
    (lambda (strs)
      (let ((ptr (unbox (malloc (* (ftype-sizeof char)
  				   (apply +
					  (map (lambda (s) (+ 1 (string-length s)))
					       strs)))))))
  	(let loop ((i 0)
  		   (s strs))
	  (write "loop daa:")
	  (write i)
	  (write s)
	  (write "\n")
  	  (cond
  	   ((null? s) ptr)
  	   (else (let ((len (string-length (car s))))
		   (string->ptr (car s) ptr i)
		   (loop (+ i len 1) (cdr s)))))))))

  ;; (define strs (list "hello" "world"))
  ;; (strings->c-array strs)


  (define ptr->strings
    (case-lambda
     ((ptr-info)
      (ptr->strings (cdr ptr-info) (car ptr-info)))
     ((ptr count)
      (let ((ptr (cond
		  ((ftype-pointer? ptr) ptr)
		  (else (make-ftype-pointer uptr ptr)))))
	(let lp ((i 0)
		 (strs (list)))
	  (cond
	   ((= i count) (reverse strs))
	   (else (lp (+ 1 i)
		     (cons (ptr->string (ftype-ref uptr () ptr i))
			   strs)))))))))

  
  (define strdup (foreign-procedure "strdup" (string) string))

  (define-syntax write-cstring
    (syntax-rules ()
      ((_ ptr ftype field-accessor str)
       (ftype-set! ftype field-accessor ptr (string->ptr str)))))

  (define-syntax read-int
    (syntax-rules ()
      ((_ ptr) (ftype-ref int () ptr))))

  (define-syntax read-unsigned-32
    (syntax-rules ()
      ((_ ptr) (ftype-ref unsigned-32 () ptr))))


  (define-syntax make-foreign-object
    (syntax-rules ()
      ((_ struct)  (make-ftype-pointer struct (unbox (malloc (ftype-sizeof struct)))))))

  (define-syntax make-foreign-array
    (syntax-rules ()
      ((_ struct size) (make-ftype-pointer struct (unbox (malloc (* size
								    (ftype-sizeof struct))))))))

  (define-syntax pointer-ref-value
    (syntax-rules ()
      ((_ ptr) (foreign-ref 'uptr (ftype-pointer-address ptr) 0))))

  (define-syntax null-pointer
    (syntax-rules ()
      ((_ type) (make-ftype-pointer type 0))))

  (meta define-syntax define-ptr-lambda 
	(syntax-rules ()
	  ((_ lambda-name struct-name member-spec f)
	   (define lambda-name
	     (lambda (obj)
	       (cond
		((ftype-pointer? struct-name obj)
		 (let ((value (f struct-name member-spec obj)))
		   (if (ftype-pointer? char value)
		       (ptr->string value)
		       value)))
		(else (raise (ffi-condition "invalid pointer" obj lambda-name)))))))))


  (define-syntax with-syntax*
    (lambda (stx)
      (syntax-case stx (values)
	((_ () body ...)
	 #'(let-values () body ...))
	((recur (((values . hd) e) . rest) body ...)
	 #'(let-values ((hd e))
	     (recur rest body ...)))
	((recur (hd . rest) body ...)
	 #'(with-syntax (hd)
	     (recur rest body ...))))))

  ;;;;;;;;;;;;;;;;;;;;
  ;; array pointer  ;;
;;;;;;;;;;;;;;;;;;;;

  (define-record-type array-pointer (fields length raw-ptr type) (nongenerative))

  ;; this function covers a general pattern for vulkan functions to return an array
  ;; the f provided will be called two times:
  ;; once for the value of count and then
  ;; after allocation of pointer-type array for the count size to
  ;; read the results in the pointer
  ;; finally, a cons pair of count and ftype-pointer is returned
  (trace-define-syntax with-new-array-pointer
    (syntax-rules ()
      ((_ pointer-type f)
       (let ((count (make-foreign-object unsigned-32)))
	 (f count (make-ftype-pointer pointer-type 0))
	 (let ((arr (make-foreign-array pointer-type (read-unsigned-32 count))))
	   (f count arr)
	   (make-array-pointer (read-unsigned-32 count) arr #'pointer-type))))))


  (define array-pointer-empty?
    (lambda (array-ptr)
      (equal? 0 (array-pointer-length array-ptr))))


  (trace-define-syntax define-collection-lambdas
    (lambda (stx)
      (syntax-case stx ()
	[(_ pointer-type)
	 (with-syntax ((map-lambda (construct-name #'pointer-type
						   #'pointer-type "-pointer-map"))
		       (find-lambda (construct-name #'pointer-type
						    #'pointer-type "-pointer-find"))
		       (for-each-lambda (construct-name #'pointer-type
							#'pointer-type "-pointer-for-each"))
		       (car-lambda (construct-name #'pointer-type
						   #'pointer-type "-pointer-car"))
		       (list->array-pointer-lambda (construct-name #'pointer-type
								   "list->"
								   #'pointer-type
								   "-pointer-array"))
		       )
	   #'(begin
	       (define map-lambda
		 (lambda (f arr-ptr)
		   (let lp ((i 0)
			    (xs '()))
		     (cond
		      ((= i (array-pointer-length arr-ptr)) (reverse xs))

		      (else (lp (fx+ 1 i)
				(cons (f (ftype-&ref pointer-type
						     ()
						     (array-pointer-raw-ptr arr-ptr)
						     i))
				      xs)))))))

	       (define find-lambda
		 (lambda (f arr-ptr)
		   (let lp ((i 0))
		     (cond
		      ((= i (array-pointer-length arr-ptr)) #f)

		      (else (let ((e (ftype-&ref pointer-type
						 ()
						 (array-pointer-raw-ptr arr-ptr)
						 i)))
			      (if (f e)
				  e
				  (lp (fx+ 1 i)))))))))

	       (define car-lambda
		 (lambda (arr-ptr)
		   (if (> 1 (array-pointer-length arr-ptr))
		       #f
		       (array-pointer-raw-ptr arr-ptr))))

	       (define for-each-lambda
		 (lambda (f arr-ptr)
		   (let lp ((i 0))
		     (cond
		      ((= i (array-pointer-length arr-ptr)) arr-ptr)

		      (else (begin (f (ftype-&ref pointer-type
						  ()
						  (array-pointer-raw-ptr arr-ptr)
						  i))
				   (lp (fx+ 1 i))))))))

	       (define list->array-pointer-lambda
		 (lambda (xs)
		   (let* ((size (length xs))
			  (arr (make-foreign-array pointer-type size)))
		     (let lp ((i 0)
			      (xs xs))
		       (cond
			((or (null? xs) (= i size))
			 (make-array-pointer size arr (quote pointer-type)))

			(else (begin
				(memcpy (+ (ftype-pointer-address arr)
					   (* i (ftype-sizeof pointer-type)))
					(ftype-pointer-address (car xs))		  
					(ftype-sizeof pointer-type))
				(lp (fx+ 1 i)
				    (cdr xs)))))))))))])))
  



  (trace-define-syntax define-foreign-struct
    (lambda (stx)

      ;; this contains a map of struct-name and its members as a compile time property
      ;; this is generated from this macro for each struct using define-property
      ;;
      ;; used for generating nested setters and getters for ftypes
      (define struct-info)

      (define array-type?
	(lambda (type)
	  (and (list? type)
	     (fx=? (length type) 3)
	     (equal? 'array (car type))
	     (number? (cadr type)))))
      
      (define construct-make-def
	(lambda (struct-name member-spec member-details)
	  (define scalar-type '(unsigned-32 int uptr))
	  
	  ;; generates the setter expression for a type
	  (define struct-set-syntax
	    (lambda (type name val-expr)

	      (define gen-setter
		(lambda ()
		  
		  (with-syntax ((struct-name struct-name)
				(name name)
				(type type)
				(val-expr val-expr))
		    (let* ((type-sym (syntax->datum #'type))
			   (member-info (assoc type-sym member-details)))
		      
		      (cond
		       ;; currently handling only one level of nesting
		       ;; this resolution will not handle the case where the
		       ;; member of a member is also a struct,
		       ;;
		       ;; hence,only scalar and pointer value can be set for a member
		       (member-info
			(map (lambda (member-spec)
			       (with-syntax* ((field-name (datum->syntax #'name
									 (car member-spec)))
					      (field-getter (construct-name #'name
									    #'type
									    "-"
									    #'field-name)))
				 #'(ftype-set! struct-name
					       (name field-name)
					       obj
					       (field-getter val-expr))))
			     (cdr member-info)))

		       ;; array type
		       ;; val-expr is expected to be a list
		       ((array-type? type-sym)
			#'(map-indexed (lambda (val i)
					 (ftype-set! struct-name (name i) obj val))
				       val-expr))
		       
		       (else (list #'(ftype-set! struct-name (name) obj val-expr))))))))
	      
	      (with-syntax* ((struct-name struct-name)
			     (name name)
			     (val-expr val-expr)
			     ((gen-setter ...) (gen-setter)))
		(case (syntax->datum type)
		  ((uptr)
		   #'(cond
		      ((string? val-expr)
		       (write-cstring obj struct-name (name) val-expr))

		      (else gen-setter ...)))
		  
		  (else #'(begin gen-setter ...))))))
	  
	  
	  (with-syntax ([struct-name struct-name]
			[((setters ... . val-exprs) ...)
			 (map (lambda (member)
				(with-syntax* ((name (car member))
					       (type (cdr member))
					       (struct-name struct-name)
					       (val-expr (construct-name #'struct-name
									 "val-"
									 #'name))
					       ((setter-syntax ...)
						(struct-set-syntax #'type
								   #'name
								   #'val-expr)))
				  (cons #'(setter-syntax ...) #'val-expr)))
			      member-spec)])
	    #'(lambda (val-exprs ...)
		(let ((obj (make-foreign-object struct-name)))
		  setters ... ...
		  obj)))))

      (define construct-val-defs
	(lambda (struct-name member-spec member-details)

	  (define construct-ptr-lambdas
	    (lambda (lambda-suffix member-spec)
	      (with-syntax* ((struct-name struct-name)
			     (lambda-suffix lambda-suffix)
			     (member-spec member-spec)
			     (lambda-name
			      (construct-name #'struct-name
					      #'struct-name "-" #'lambda-suffix))
			     
			     (lambda-&name
			      (construct-name #'struct-name
					      #'struct-name "-&" #'lambda-suffix)))
		#'(begin
		    (define-ptr-lambda lambda-name struct-name member-spec ftype-ref)
		    (define-ptr-lambda lambda-&name struct-name member-spec ftype-&ref)))))
	  
	  (with-syntax ((struct-name struct-name))
	    (map (lambda (member)
		   (with-syntax* ((name (car member))
				  (type (cdr member)))
		     (let* ((type-sym (syntax->datum #'type))
			    (member-types (assoc type-sym member-details)))
		       (cond

			;; getters for struct fields
			;; only one level of nesting handled
			(member-types
			 (with-syntax* (((getters ...)
					 (map (lambda (member-spec)
						(with-syntax* ((field-name (datum->syntax
									    #'name
									    (car member-spec)))
							       (suffix
								(construct-name #'struct-name
										#'name
										"-"
										#'field-name)))
						  (construct-ptr-lambdas #'suffix
									 (list #'name
									       #'field-name))))
					      (cdr member-types))))
			   #'(begin getters ...)))

			;; array types
			((array-type? type-sym)
			 (with-syntax ((size (datum->syntax #'name (cadr type-sym)))
				       (lambda-name (construct-name #'name
								    #'struct-name "-" #'name)))
			   #'(define lambda-name
			       (lambda (ptr)
				 (if (ftype-pointer? struct-name ptr)
				     (map (lambda (i)
					    (ftype-ref struct-name (name i) ptr))
					  (iota size))
				     (raise (ffi-condition "invalid pointer" ptr lambda-name)))))))

			;; scalar / pointer getters
			(else (construct-ptr-lambdas #'name (list #'name)))))))
		 member-spec))))
      
      (syntax-case stx ()
	[(_ struct-name ((member-name . member-type) ...))
	 (for-all identifier? #'(struct-name member-name ...))
	 ;; to lookup compile time values, used to
	 ;; lookup fields of member structs when struct members are values
	 (lambda (lookup)
	   ;; member-details retrieves information of member-types which have been
	   ;; defined by define-foreign-struct
	   ;; this is used to generate the ftype-ref! calls
	   (let ((member-details (filter identity
					 (map (lambda (type)
						(let ((members (and (identifier? type)
								    (lookup type #'struct-info))))
						  (cond
						   (members
						    (cons (syntax->datum type) members))
						   (else #f))))
					      #'(member-type ...)))))
	     
	     (with-syntax ([struct-name #'struct-name]
			   [make-struct-name (construct-name #'struct-name "make-" #'struct-name)]
			   [make-struct-def
			    (construct-make-def #'struct-name
						#'((member-name . member-type) ...)
						member-details)]
			   [(val-defs ...)
			    (construct-val-defs #'struct-name
						#'((member-name . member-type) ...)
						member-details)])
	       #'(begin
		   (define-ftype struct-name (struct [member-name member-type] ...))
		   (define-property struct-name struct-info '((member-name . member-type) ...))
		   (define make-struct-name make-struct-def)
		   val-defs ...))))])))

  ;; ref: https://www.cs.indiana.edu/~dyb/pubs/ftypes.pdf
  (define-syntax (define-enum-ftype x)
    (define (enum e*)
      (let f ([e* e*] [n 0])
	(if (null? e*)
	    '()
	    (syntax-case (car e*) ()
	      [(e v)
	       (cons #'(e v)
		     (f (cdr e*)
			(+ (syntax->datum #'v) 1)))]
	      [e (identifier? #'e)
		 (cons #`(e #,n)
		       (f (cdr e*) (+ n 1)))]))))
    (syntax-case x ()
      [(_ name e* ...)
       (with-syntax ([((e v) ...) (enum #'(e* ...))])
	 #'(begin
	     (define-ftype name int)
	     (define e v) ...))]))

  ;; configure gc
  ;; (collect-request-handler (lambda ()
  ;; 			     (collect)
  ;; 			     (let lp ()
  ;; 			       (let ((x (malloc-guardian)))
  ;; 				 (when x
  ;; 				   (do-free x)
  ;; 				   (lp))))))
  ;; 
  )
#|

--------------------------------------------

(load "ffi.scm")

(import (ffi))

(define sample (load-shared-object "./sample.so"))

(define csum (foreign-procedure "sum" (int int) int))
(define s (foreign-procedure "retString" () uptr))

(ptr->strings (make-ftype-pointer uptr (s)) 3)

(define-foreign-struct abc
  ((a . int)
   (e . asas)
   (d . (* (* char)))))

(define strs '("hello" "world"))

(define a "hello")

(define strs (list a "do it" "people" "iuyt"))

(ftype-pointer? uptr (string->ptr "abcd"))

;; convert to test
(ptr->string (string->ptr "ancd"))

(strings->ptr strs)

(define a (cdr exts))

(trace ptr->strings)
(ptr->string (strings->c-array (ptr->strings (strings->ptr strs) (length strs))))

(import (glfw))
(glfw-init)
(define exts (glfw-get-required-instance-extensions))

(ptr->strings  (strings->c-array (ptr->strings exts)))


(foreign-address-name (cdr exts))
(equal? a (ptr->string (string->ptr a)))

|#
