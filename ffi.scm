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

	  malloc
	  make-foreign-object
	  define-foreign-struct
	  define-enum-ftype
	  cstring)

  (import (chezscheme))

  (define libc (load-shared-object "libc.so.6"))
  
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

  (trace-define-syntax define-foreign-struct
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
					     (val-expr (ftype-set! struct-name (name) obj val-expr)))
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
