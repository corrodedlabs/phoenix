(library (phoenix image)
  (export with-image)
  (import (scheme)
	  (ffi))

  (define o (load-shared-object "../phoenix-libs/libstb_image.so"))

  (define-enum-ftype desired-channels
    stbi-default stbi-grey stbi-grey-alpha stbi-rgb stbi-rgb-alpha)

  (define-ftype stbi-uc uptr)

  (define stbi_load
    (foreign-procedure "stbi_load" (string (* int) (* int) (* int) desired-channels) stbi-uc))

  ;; stbi_free is just free
  (define stbi_free (foreign-procedure "free" (stbi-uc) void))

  (define with-image
    (lambda (image-path f)
      (let ((image #f))
	(dynamic-wind
	    (lambda ()
	      (let ((width (make-foreign-object int))
		    (height (make-foreign-object int))
		    (channels (make-foreign-object int)))
		(set! image (cons (stbi_load image-path width height channels stbi-rgb-alpha)
				  (fx* (read-int width) (read-int height) 4)))))
	    (lambda () (f image))
	    (lambda ()
	      (display "free image") (newline)
	      (stbi_free (car image))))))))
