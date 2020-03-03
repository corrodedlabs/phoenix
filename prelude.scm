(library (prelude)
  
  (export identity
	  construct-name
	  map-indexed
	  kebab-case->camel-case)

  (import (chezscheme))

  (define identity (lambda (x) x))

  (define construct-name
    (lambda (template-identifier . args)
      (datum->syntax template-identifier
	(string->symbol (apply string-append (map (lambda (x)
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

  (define kebab-case->camel-case
    (lambda (str)
      (let lp ((str  (string->list str))
	       (dest '()))
	(cond
	 ((null? str) (list->string (reverse dest))) 
	 
	 ((char=? (car str) #\-) (lp (cddr str)
				     (cons (char-upcase (cadr str)) dest)))
	 
	 (else (lp (cdr str)
		   (cons (car str) dest))))))))


;; (kebab-case->camel-case "vk-cmd-begin")

;; (map-indexed (lambda (e i) (+ e i)) '(12 34 5))
