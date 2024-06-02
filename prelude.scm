(library (prelude)

  (export identity
	  construct-name
	  displayln
	  map-indexed
	  take
	  kebab-case->camel-case
	  interleave
	  interpose
	  mapcat
	  clamp-between)

  (import (chezscheme)
	  (srfi s41 streams))

  (define identity (lambda (x) x))

  (define construct-name
    (lambda (template-identifier . args)
      (datum->syntax template-identifier
		     (string->symbol
		      (apply string-append
			     (map (lambda (x)
				    (if (string? x)
					x
					(symbol->string (syntax->datum x))))
				  args))))))

  (define-syntax displayln
    (syntax-rules ()
      ((_ str ...)
       (begin (display str ...) (newline)))))

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
    (lambda (pos list)
      (let loop ((i 0) (xs list) (res '()))
	(cond
	 ((or (null? xs) (= i pos)) (reverse res))
	 (else (loop (+ i 1)
		     (cdr xs)
		     (cons (car xs) res)))))))

  (define kebab-case->camel-case
    (lambda (str)
      (let lp ((str (string->list str))
	       (dest '()))
	(cond
	 ((null? str) (list->string (reverse dest)))

	 ((char=? (car str) #\-)
	  (lp (cddr str)
	      (cons (char-upcase (cadr str)) dest)))

	 (else (lp (cdr str) (cons (car str) dest)))))))

  (define mapcat (lambda (f xs) (apply append (map f xs))))

  (define interleave
    (lambda (c1 c2)
      (if (or (null? c1) (null? c2))
	  '()
	  (cons (car c1)
		(cons (car c2) (interleave (cdr c1) (cdr c2)))))))

  (define interpose
    (lambda (sep coll)
      (let loop ((c coll) (res '()))
	(cond
	 ((null? c) (reverse (cdr res)))
	 (else (loop (cdr c)
		     (cons sep (cons (car c) res))))))))

  (define clamp-between
    (lambda (min-clamp num max-clamp)
      (max min-clamp (min max-clamp num)))))

#|
----------------------------------

(load "./prelude.scm")
(import (prelude))

(define-syntax test
(syntax-rules ()
((_ l r) (assert (equal? l r)))))

(test (take 2 '(10 20 30 40)) '(10 20))
(test (kebab-case->camel-case "vk-cmd-begin") "vkCmdBegin")
(test (interleave '(1 2 3 4)
'(9 8 7 6)) '(1 9 2 8 3 7 4 6))
(test (interpose "," '(1 2 3)) '(1 "," 2 "," 3))
|#
