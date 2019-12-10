(library (ffi)
  
  (export write-cstring
	  ptr->string
	  string->ptr
	  strdup
	  char**
	  char**->strings
	  strings->char**
	  read-int
	  malloc
	  make-foreign-object
	  define-foreign-struct
	  define-enum-ftype)

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
    
  (define strdup (foreign-procedure "strdup" (string) string))

  (define-syntax write-cstring
    (syntax-rules ()
      ((_ ptr ftype field-accessor str)
       (ftype-set! ftype field-accessor ptr (strdup str)))))

  (define-syntax read-int
    (syntax-rules ()
      ((_ ptr) (ftype-ref int () ptr))))


  (define ptr->string
    (lambda (ptr)
      (let loop ((i 0)
		 (chs '()))
	(let ((ch (foreign-ref 'char (ftype-pointer-address ptr) i)))
	  (cond
	   ((char=? ch #\nul) (list->string (reverse chs)))
	   (else (loop (+ i 1)
		       (cons ch chs))))))))

  (define (string->ptr str)
    (let ((ptr (unbox (malloc (* (ftype-sizeof char)
				 (string-length str))))))
      (let lp ((i 0))
	(cond
	 ((= i (string-length str))
	  (foreign-set! 'char ptr i #\nul)
	  (make-ftype-pointer uptr ptr))
	 (else (foreign-set! 'char ptr i (string-ref str i))
	       (lp (+ 1 i)))))))

  (define-ftype char** (* (* char)))

  (define (char**->strings ptr count)
    (let lp ((i 0)
	     (strs '()))
      (cond
       ((< i count)
	(lp (+ 1 i)
	    (cons (ptr->string (ftype-ref char** (i) ptr)) strs)))
       (else strs))))

  (define (strings->char** strs)
    (let ((ptr (make-ftype-pointer uptr (unbox (malloc (* (ftype-sizeof char)
							  (apply + (map (lambda (s)
									  (+ 1 (string-length s)))
									strs))))))))
      (let lp ((i 0)
	       (s strs))
	(cond
	 ((null? s) ptr)
	 (else (begin (ftype-set! uptr () ptr i (strdup (car s)))
		      (lp (+ 1 i) (cdr s))))))))

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
  (collect-request-handler (lambda ()
			     (collect)
			     (let lp ()
			       (let ((x (malloc-guardian)))
				 (when x
				   (do-free x)
				   (lp)))))))

#!eof

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

(ftype-pointer-ftype (strings->char** strs))

(ftype-sizeof char**)

(strdup "abcd")

(define o (make-abc 12 (strings->char** strs)))


--------------------------------------------

(define a "hello")

(define strs (list a "do it" "people" "iuyt"))
;; (define strings->ptr
;;   (lambda (strs)
;;     (let ((ptr (make-ftype-pointer uptr
;; 				   (unbox
;; 				    (malloc (* (ftype-sizeof uptr)
;; 					       (length strs)))))))
;;       (let lp ((i 0)
;; 	       (s strs))
;; 	(cond
;; 	 ((null? s) ptr)
;; 	 (else (ftype-set! uptr () ptr i (string->ptr (car strs)))
;; 	       (lp (+ 1 i) (cdr s))))))))

(define strings->ptr
  (lambda (strs)
    (let ((ptr (unbox (malloc (* (ftype-sizeof uptr)
				 (length strs))))))
      (let lp ((i 0)
	       (s strs))
	(write i)
	(write s)
	(cond
	 ((null? s) (make-ftype-pointer uptr ptr))
	 (else (foreign-set! 'uptr ptr i (ftype-pointer-address (string->ptr (car s))))
	       (lp (+ 1 i) (cdr s))))))))

(strings->ptr strs)

(define ptr->strings
  (lambda (ptr count)
    (let lp ((i 0)
	     (strs (list)))
      (cond
       ((= i count) (reverse strs))
       (else (lp (+ 1 i)
		 (cons (ptr->string
			(make-ftype-pointer uptr
					    (ftype-ref uptr () ptr i)))
		       strs)))))))

(define a (cdr exts))

(trace ptr->strings)
(ptr->strings (strings->ptr strs) (length strs))

(ptr->strings (cdr exts) 2)

(define exts (glfw-get-required-instance-extensions))




(equal? a (ptr->string (string->ptr a)))
