(library (renderdoc)
  (export get-api-version
	  start-frame-capture
	  trigger-capture
	  end-frame-capture)

  (import (scheme)
	  (ffi)
	  (prelude))

  (define x (load-shared-object "librenderdoc.so"))

  (define-enum-ftype renderdoc-api-version
    (erenderdoc-api-version-1-0-0  10000)    
    (erenderdoc-api-version-1-0-1  10001)    
    (erenderdoc-api-version-1-0-2  10002)    
    (erenderdoc-api-version-1-1-0  10100)    
    (erenderdoc-api-version-1-1-1  10101)    
    (erenderdoc-api-version-1-1-2  10102)    
    (erenderdoc-api-version-1-2-0  10200)    
    (erenderdoc-api-version-1-3-0  10300)    
    (erenderdoc-api-version-1-4-0  10400)    
    (erenderdoc-api-version-1-4-1  10401))

  ;; // Shutdown was renamed to RemoveHooks in 1.4.1.
  ;; // These unions allow old code to continue compiling without changes
  (define-ftype remove-hooks
    (union (shutdown uptr)
	   (remove-hooks uptr)))

  ;; // Get/SetLogFilePathTemplate was renamed to Get/SetCaptureFilePathTemplate in 1.1.2.
  ;; // These unions allow old code to continue compiling without changes
  (define-ftype set-capture-file-path-template
    (union (set-log-file-path-template uptr)
	   (set-capture-file-path-template uptr)))

  (define-ftype get-capture-file-path-template
    (union (get-log-file-path-template uptr)
	   (get-capture-file-path-template uptr)))

  ;; // IsRemoteAccessConnected was renamed to IsTargetControlConnected in 1.1.1.
  ;; // This union allows old code to continue compiling without changes
  (define-ftype is-target-control-connected
    (union (is-remote-access-connected uptr)
	   (is-target-control-connected uptr)))


  (define-ftype get-api-version-ftype (function ((* int) (* int) (* int)) void))

  (define-ftype start-frame-capture-ftype (function (uptr uptr) void))
  (define-ftype is-frame-capturing-ftype (function () unsigned-32))
  (define-ftype end-frame-capture-ftype (function (uptr uptr) void))
  (define-ftype trigger-capture-ftype (function () void))

  (define-ftype render-doc-api
    (struct (get-api-version uptr)

	    (set-capture-option-u32 uptr)
	    (set-capture-option-f32 uptr)

	    (get-capture-option-u32 uptr)
	    (get-capture-option-f32  uptr)

	    (set-focus-toggle-keys uptr)
	    (set-capture-keys uptr)

	    (get-overlay-bits uptr)
	    (mask-overlay-bits uptr)

	    (remove-hooks uptr)
	    (unload-crash-handler uptr)

	    (set-capture-file-path-template uptr)
	    (get-capture-file-path-template uptr)

	    (get-num-captures uptr)
	    (get-capture uptr)

	    (trigger-capture uptr)
	    (is-target-control-connected uptr)

	    (launch-replay-ui uptr)

	    (set-active-window uptr)

	    (start-frame-capture uptr)
	    (is-frame-capturing uptr)
	    (end-frame-capture uptr)

	    ;; // new function in 1.1.0
	    (trigger-multi-frame-capture uptr)

	    ;; // new function in 1.2.0
	    (set-capture-file-comments uptr)

	    ;; // new function in 1.4.0
	    (discard-frame-capture uptr)))


  ;; (define-ftype rr (struct (rapi (* render-doc-api))))

  (define get-renderdoc-api
    (foreign-procedure "RENDERDOC_GetAPI" (renderdoc-api-version uptr) int))


  (define api-obj
    (begin (let ((api-struct (make-foreign-object uptr)))
	     (make-ftype-pointer render-doc-api (pointer-ref-value api-struct)))))


  (define-syntax (define-renderdoc-function stx)
    (syntax-case stx ()
      ((_ field-name api-ptr)
       (with-syntax ((function-name (construct-name #'field-name #'field-name "-func"))
		     (ftype (construct-name #'field-name #'field-name "-ftype")))
	 #'(define function-name
	     (ftype-ref ftype
			()
			(make-ftype-pointer ftype
					    (ftype-ref render-doc-api (field-name) api-ptr))))))))


  (define-renderdoc-function get-api-version api-obj)
  (define-renderdoc-function is-frame-capturing api-obj)
  (define-renderdoc-function start-frame-capture api-obj)
  (define-renderdoc-function trigger-capture api-obj)
  (define-renderdoc-function end-frame-capture api-obj)

  (define get-api-version
    (lambda ()
      (let ((major (make-foreign-object int))
	    (minor (make-foreign-object int))
	    (patch (make-foreign-object int)))
	(get-api-version-func major minor patch)
	(list (read-int major) (read-int minor) (read-int patch)))))

  (define start-frame-capture
    (lambda ()
      (start-frame-capture-func 0 0)))

  (define is-capturing-frame? is-frame-capturing-func)
  (define trigger-capture trigger-capture-func)
  (define end-frame-capture end-frame-capture-func))
