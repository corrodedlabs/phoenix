(library (prelude)
  
  (export identity
	  construct-name
	  map-indexed
	  take
	  kebab-case->camel-case

	  define-interface
	  define-module
	  abstract-module
	  implement)

  (import (chezscheme))

  (define identity (lambda (x) x))

  (define construct-name
    (lambda (template-identifier . args)
      (datum->syntax template-identifier
	(string->symbol (apply string-append
			  (map (lambda (x)
				 (if (string? x)
				   x
				   (symbol->string (syntax->datum x))))
			       args))))))

  (define map-indexed
    (lambda (f arr)
      (let lp ((i 0)
	       (xs arr)
	       (coll '()))
	(cond
	 ((null? xs) (reverse coll))
	 (else (lp (+ i 1)
		   (cdr xs)
		   (cons (f (car xs) i) coll)))))))

  (define take
    (lambda (list pos)
      (let ((vec (list->vector list)))
	(map (lambda (i) (vector-ref vec i)) (iota pos)))))

  (define kebab-case->camel-case
    (lambda (str)
      (let lp ((str  (string->list str))
	       (dest '()))
	(cond
	 ((null? str) (list->string (reverse dest))) 
	 
	 ((char=? (car str) #\-) (lp (cddr str)
				     (cons (char-upcase (cadr str)) dest)))
	 
	 (else (lp (cdr str)
		   (cons (car str) dest)))))))

  ;; Modules

  ;; taken from chez scheme user guide

  
  ;; To allow interfaces to be separated from implementations, the following syntactic
  ;; abstractions support the definition and use of named interfaces.

  ;; define-interface creates an interface macro that, given a module name and a list of
  ;; definitions, expands into a module definition with a concrete interface.

  ;; with-implicit is used to ensure that the introduced export identifiers are visible in
  ;; the same scope as the name of the module in the define-module form.

  ;; define-interface and define-module can be used as follows.

  ;; (define-interface simple (a b))
  ;; (define-module m simple
  ;;   (define-syntax a (identifier-syntax 1))
  ;;   (define b (lambda () c))
  ;;   (define c 2))
  ;; (let () (import m) (+ a (b))) <graphic> 3

  ;; The abstract module facility defined below allows a module interface to be satisfied
  ;; incrementally when module forms are evaluated. This permits flexibility in the
  ;; separation between the interface and implementation, supports separate compilation of
  ;; mutually recursive modules, and permits redefinition of module implementations.

  (define-syntax define-interface
    (syntax-rules ()
      [(_ name (export ...))
       (define-syntax name
	 (lambda (x)
	   (syntax-case x ()
	     [(_ n defs)
	      (with-implicit (n export ...)
			     #'(module n (export ...) .
				       defs))])))]))

  (define-syntax define-module
    (syntax-rules ()
      [(_ name interface defn ...)
       (interface name (defn ...))]))

  ;; Within an abstract-module form, each of the exports in the list ex ... must be
  ;; variables.
  ;; The values of these variables are supplied by one or more separate implement forms.
  ;; Since keyword bindings must be present at compile time, they cannot be satisfied
  ;; incrementally and are instead listed as separate exports and defined within the
  ;; abstract module.

  ;; Within an implement form, the sequence of forms form ... is a sequence of zero or more
  ;; definitions followed by a sequence of zero or more expressions.
  ;; Since the module used in the expansion of implement does not export anything,
  ;; the definitions are all local to the implement form. The expressions may be arbitrary
  ;; expressions, but should include one satisfy form for each variable whose definition is
  ;; supplied by the implement form. A satisfy form has the syntax

  ;; (satisfy variable expr)

  ;; declare and satisfy may simply be the equivalents of define and set!.

  ;; (define-syntax declare (identifier-syntax define))
  ;; (define-syntax satisfy (identifier-syntax set!))

  ;; Alternatively, declare can initialize the declared variable to the value of a flag known
  ;; only to declare and satisfy, and satisfy can verify that this flag is still present to
  ;; insure that only one attempt to satisfy the value of a given identifier is made.

  ;; (module ((declare cookie) (satisfy cookie))
  ;; 	  (define cookie "chocolate chip")
  ;; 	  (define-syntax declare
  ;; 	    (syntax-rules () [(_ var) (define var cookie)]))
  ;; 	  (define-syntax satisfy
  ;; 	    (syntax-rules ()
  ;; 	      [(_ var exp)
  ;; 	       (if (eq? var cookie)
  ;; 		   (set! var exp)
  ;; 		   (assertion-violationf 'satisfy
  ;; 					 "value of variable ~s has already been satisfied"
  ;; 					 'var))])))

  ;; Using abstract-module and implement, we can define mutually recursive and separately
  ;; compilable modules as follows.

  ;; (abstract-module e (even?) (pred)
  ;; 		   (define-syntax pred
  ;; 		     (syntax-rules () [(_ exp) (- exp 1)])))

  ;; (abstract-module o (odd?) ())

  ;; (implement e
  ;; 	     (import o)
  ;; 	     (satisfy even?
  ;; 		      (lambda (x)
  ;; 			(or (zero? x) (odd? (pred x))))))

  ;; (implement o
  ;; 	     (import e)
  ;; 	     (satisfy odd?
  ;; 		      (lambda (x) (not (even? x)))))

  ;; (let () (import-only e) (even? 38)) <graphic> #t

  (define-syntax abstract-module
    (syntax-rules ()
      [(_ name (ex ...) (kwd ...) defn ...)
       (module name (ex ... kwd ...)
	       (declare ex) ...
	       defn ...)]))

  (define-syntax implement
    (syntax-rules ()
      [(_ name form ...)
       (module () (import name) form ...)])))


;; (kebab-case->camel-case "vk-cmd-begin")

;; (map-indexed (lambda (e i) (+ e i)) '(12 34 5))
