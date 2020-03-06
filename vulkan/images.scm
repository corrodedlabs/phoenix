
;; Images

;; for swapchain these are created automatically
;; for textures and depth buffers, these have to be created manually


;; find supported formats out of candidate format with the given
;; tiling and features
(define find-supported-formats
  (lambda (physical-device candidate-formats tiling features)

    (define get-format-properties
      (lambda (format)
	(let ((format-properties (make-foreign-object vk-format-properties)))
	  (vk-get-physical-device-format-properties physical-device
						    format
						    format-properties)
	  format-properties)))
    
    (find (lambda (format)
	    (let ((props (get-format-properties format)))
	      (or (and (equal? tiling vk-image-tiling-linear)
		    (equal? features (bitwise-and
				      (vk-format-properties-linear-tiling-features props)
				      features)))
		 (and (equal? tiling vk-image-tiling-optimal)
		    (equal? features (bitwise-and
				      (vk-format-properties-optimal-tiling-features props)
				      features))))))
	  candidate-formats)))

;; There are several formats that fit this requirement:

;; VK_FORMAT_D32_SFLOAT: 32-bit float for depth
;; VK_FORMAT_D32_SFLOAT_S8_UINT: 32-bit signed float for depth and 8 bit stencil component
;; VK_FORMAT_D24_UNORM_S8_UINT: 24-bit float for depth and 8 bit stencil component
;;
(define candidate-formats (list vk-format-d32-sfloat
				vk-format-d32-sfloat-s8-uint
				vk-format-d24-unorm-s8-uint))

(define supported-format
  (find-supported-formats physical-device
			  candidate-formats
			  vk-image-tiling-optimal
			  vk-format-feature-depth-stencil-attachment-bit))

;; Image Properties required to create a image
(define-record-type image-properties
  (fields width height format usage-flags aspect-flags))

(define create-image-handle
  (lambda (device image-properties)
    (let* ((extent (make-vk-extent-3d (image-properties-width image-properties)
				      (image-properties-height image-properties)
				      1))
	   (info
	    (make-vk-image-create-info image-create-info
				       0
				       0
				       vk-image-type-2d
				       (image-properties-format image-properties)
				       extent
				       1
				       1
				       vk-sample-count-1-bit
				       vk-image-tiling-optimal
				       (image-properties-usage-flags image-properties)
				       vk-sharing-mode-exclusive
				       0
				       (null-pointer u32)
				       vk-image-layout-undefined))
	   (image (make-foreign-object vk-image)))
      (vk-create-image device info 0 image)
      image)))


(define create-depth-property
  (lambda (extent format)
    (make-image-properties (vk-extent-2d-width extent)
			   (vk-extent-2d-height extent)
			   format
			   vk-image-usage-depth-stencil-attachment-bit
			   vk-image-aspect-depth-bit)))


(define extent (swapchain-extent swapchain))

(define depth-property (create-depth-property extent supported-format))

(define depth-image-handle (create-image-handle device depth-property))

