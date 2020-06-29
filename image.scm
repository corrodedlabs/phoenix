(library (image)
  (export with-image
	  image-data-pointer
	  image-data-width
	  image-data-height
	  image-data-size
	  image-data?)
  (import (scheme)
	  (prelude)
	  (ffi))

  (define o (load-shared-library (make-library-detail #f
						      "phoenix-libs/libstb_image.so"
						      "phoenix-libs/libstb_image.dll")))

  (define-enum-ftype desired-channels
    stbi-default stbi-grey stbi-grey-alpha stbi-rgb stbi-rgb-alpha)

  (define-ftype stbi-uc uptr)

  (define stbi_load
    (foreign-procedure "stbi_load" (string (* int) (* int) (* int) desired-channels) stbi-uc))

  ;; stbi_free is just free
  (define stbi_free (foreign-procedure "free" (stbi-uc) void))

  (define-record-type image-data (fields pointer width height size))

  (define with-image
    (lambda (image-path f)
      ;; image is image-data
      (let ((image #f))
	(dynamic-wind
	  (lambda ()
	    (let ((width (make-foreign-object int))
		  (height (make-foreign-object int))
		  (channels (make-foreign-object int)))
	      (set! image
		(make-image-data (stbi_load image-path width height channels stbi-rgb-alpha)
				 (read-int width)
				 (read-int height)
				 (fx* (read-int height) (read-int width) 4)))))
	  (lambda () (f image))
	  (lambda ()
	    (display "free image") (newline)
	    (stbi_free (image-data-pointer image))))))))
