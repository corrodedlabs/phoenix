(trace-define-syntax define-enum-library
  (lambda (stx)
    (define (identifiers e*)
      (let lp ((e* e*)
	       (ids '()))
	(cond
	 ((null? e*) ids)
	 (else 
	  (syntax-case (car e*) ()
	    ((e _) (lp (cdr e*)
		       (cons #'e ids)))
	    (e (lp (cdr e*)
		   (cons #'e ids))))))))
    
    (syntax-case stx ()
      ((_ library-name enum-name e* ...)
       (with-syntax (((e ...) (identifiers #'(e* ...))))
	 (syntax (library library-name
		   (export enum-name e ...)
		   (import (ffi))
		   (define-enum-ftype enum-name e* ...))))))))

(define-enum-library (vulkan structure-types) vk-structure-type
  (application-info  0)
  (instance-create-info  1)
  (device-queue-create-info  2)
  (device-create-info  3)
  (submit-info  4)
  (memory-allocate-info  5)
  (mapped-memory-range  6)
  (bind-sparse-info  7)
  (fence-create-info  8)
  (semaphore-create-info  9)
  (event-create-info  10)
  (query-pool-create-info  11)
  (buffer-create-info  12)
  (buffer-view-create-info  13)
  (image-create-info  14)
  (image-view-create-info  15)
  (shader-module-create-info  16)
  (pipeline-cache-create-info  17)
  (pipeline-shader-stage-create-info  18)
  (pipeline-vertex-input-state-create-info  19)
  (pipeline-input-assembly-state-create-info  20)
  (pipeline-tessellation-state-create-info  21)
  (pipeline-viewport-state-create-info  22)
  (pipeline-rasterization-state-create-info  23)
  (pipeline-multisample-state-create-info  24)
  (pipeline-depth-stencil-state-create-info  25)
  (pipeline-color-blend-state-create-info  26)
  (pipeline-dynamic-state-create-info  27)
  (graphics-pipeline-create-info  28)
  (compute-pipeline-create-info  29)
  (pipeline-layout-create-info  30)
  (sampler-create-info  31)
  (descriptor-set-layout-create-info  32)
  (descriptor-pool-create-info  33)
  (descriptor-set-allocate-info  34)
  (write-descriptor-set  35)
  (copy-descriptor-set  36)
  (framebuffer-create-info  37)
  (render-pass-create-info  38)
  (command-pool-create-info  39)
  (command-buffer-allocate-info  40)
  (command-buffer-inheritance-info  41)
  (command-buffer-begin-info  42)
  (render-pass-begin-info  43)
  (buffer-memory-barrier  44)
  (image-memory-barrier  45)
  (memory-barrier  46)
  (loader-instance-create-info  47)
  (loader-device-create-info  48)
  (physical-device-subgroup-properties  1000094000)
  (bind-buffer-memory-info  1000157000)
  (bind-image-memory-info  1000157001)
  (physical-device-16bit-storage-features  1000083000)
  (memory-dedicated-requirements  1000127000)
  (memory-dedicated-allocate-info  1000127001)
  (memory-allocate-flags-info  1000060000)
  (device-group-render-pass-begin-info  1000060003)
  (device-group-command-buffer-begin-info  1000060004)
  (device-group-submit-info  1000060005)
  (device-group-bind-sparse-info  1000060006)
  (bind-buffer-memory-device-group-info  1000060013)
  (bind-image-memory-device-group-info  1000060014)
  (physical-device-group-properties  1000070000)
  (device-group-device-create-info  1000070001)
  (buffer-memory-requirements-info-2  1000146000)
  (image-memory-requirements-info-2  1000146001)
  (image-sparse-memory-requirements-info-2  1000146002)
  (memory-requirements-2  1000146003)
  (sparse-image-memory-requirements-2  1000146004)
  (physical-device-features-2  1000059000)
  (physical-device-properties-2  1000059001)
  (format-properties-2  1000059002)
  (image-format-properties-2  1000059003)
  (physical-device-image-format-info-2  1000059004)
  (queue-family-properties-2  1000059005)
  (physical-device-memory-properties-2  1000059006)
  (sparse-image-format-properties-2  1000059007)
  (physical-device-sparse-image-format-info-2  1000059008)
  (physical-device-point-clipping-properties  1000117000)
  (render-pass-input-attachment-aspect-create-info  1000117001)
  (image-view-usage-create-info  1000117002)
  (pipeline-tessellation-domain-origin-state-create-info  1000117003)
  (render-pass-multiview-create-info  1000053000)
  (physical-device-multiview-features  1000053001)
  (physical-device-multiview-properties  1000053002)
  (physical-device-variable-pointers-features  1000120000)
  (protected-submit-info  1000145000)
  (physical-device-protected-memory-features  1000145001)
  (physical-device-protected-memory-properties  1000145002)
  (device-queue-info-2  1000145003)
  (sampler-ycbcr-conversion-create-info  1000156000)
  (sampler-ycbcr-conversion-info  1000156001)
  (bind-image-plane-memory-info  1000156002)
  (image-plane-memory-requirements-info  1000156003)
  (physical-device-sampler-ycbcr-conversion-features  1000156004)
  (sampler-ycbcr-conversion-image-format-properties  1000156005)
  (descriptor-update-template-create-info  1000085000)
  (physical-device-external-image-format-info  1000071000)
  (external-image-format-properties  1000071001)
  (physical-device-external-buffer-info  1000071002)
  (external-buffer-properties  1000071003)
  (physical-device-id-properties  1000071004)
  (external-memory-buffer-create-info  1000072000)
  (external-memory-image-create-info  1000072001)
  (export-memory-allocate-info  1000072002)
  (physical-device-external-fence-info  1000112000)
  (external-fence-properties  1000112001)
  (export-fence-create-info  1000113000)
  (export-semaphore-create-info  1000077000)
  (physical-device-external-semaphore-info  1000076000)
  (external-semaphore-properties  1000076001)
  (physical-device-maintenance-3-properties  1000168000)
  (descriptor-set-layout-support  1000168001)
  (physical-device-shader-draw-parameters-features  1000063000)
  (swapchain-create-info-khr  1000001000)
  (present-info-khr  1000001001)
  (device-group-present-capabilities-khr  1000060007)
  (image-swapchain-create-info-khr  1000060008)
  (bind-image-memory-swapchain-info-khr  1000060009)
  (acquire-next-image-info-khr  1000060010)
  (device-group-present-info-khr  1000060011)
  (device-group-swapchain-create-info-khr  1000060012)
  (display-mode-create-info-khr  1000002000)
  (display-surface-create-info-khr  1000002001)
  (display-present-info-khr  1000003000)
  (xlib-surface-create-info-khr  1000004000)
  (xcb-surface-create-info-khr  1000005000)
  (wayland-surface-create-info-khr  1000006000)
  (android-surface-create-info-khr  1000008000)
  (win32-surface-create-info-khr  1000009000)
  (debug-report-callback-create-info-ext  1000011000)
  (pipeline-rasterization-state-rasterization-order-amd  1000018000)
  (debug-marker-object-name-info-ext  1000022000)
  (debug-marker-object-tag-info-ext  1000022001)
  (debug-marker-marker-info-ext  1000022002)
  (dedicated-allocation-image-create-info-nv  1000026000)
  (dedicated-allocation-buffer-create-info-nv  1000026001)
  (dedicated-allocation-memory-allocate-info-nv  1000026002)
  (physical-device-transform-feedback-features-ext  1000028000)
  (physical-device-transform-feedback-properties-ext  1000028001)
  (pipeline-rasterization-state-stream-create-info-ext  1000028002)
  (image-view-handle-info-nvx  1000030000)
  (texture-lod-gather-format-properties-amd  1000041000)
  (stream-descriptor-surface-create-info-ggp  1000049000)
  (physical-device-corner-sampled-image-features-nv  1000050000)
  (external-memory-image-create-info-nv  1000056000)
  (export-memory-allocate-info-nv  1000056001)
  (import-memory-win32-handle-info-nv  1000057000)
  (export-memory-win32-handle-info-nv  1000057001)
  (win32-keyed-mutex-acquire-release-info-nv  1000058000)
  (validation-flags-ext  1000061000)
  (vi-surface-create-info-nn  1000062000)
  (physical-device-texture-compression-astc-hdr-features-ext  1000066000)
  (image-view-astc-decode-mode-ext  1000067000)
  (physical-device-astc-decode-features-ext  1000067001)
  (import-memory-win32-handle-info-khr  1000073000)
  (export-memory-win32-handle-info-khr  1000073001)
  (memory-win32-handle-properties-khr  1000073002)
  (memory-get-win32-handle-info-khr  1000073003)
  (import-memory-fd-info-khr  1000074000)
  (memory-fd-properties-khr  1000074001)
  (memory-get-fd-info-khr  1000074002)
  (win32-keyed-mutex-acquire-release-info-khr  1000075000)
  (import-semaphore-win32-handle-info-khr  1000078000)
  (export-semaphore-win32-handle-info-khr  1000078001)
  (d3d12-fence-submit-info-khr  1000078002)
  (semaphore-get-win32-handle-info-khr  1000078003)
  (import-semaphore-fd-info-khr  1000079000)
  (semaphore-get-fd-info-khr  1000079001)
  (physical-device-push-descriptor-properties-khr  1000080000)
  (command-buffer-inheritance-conditional-rendering-info-ext  1000081000)
  (physical-device-conditional-rendering-features-ext  1000081001)
  (conditional-rendering-begin-info-ext  1000081002)
  (physical-device-shader-float16-int8-features-khr  1000082000)
  (present-regions-khr  1000084000)
  (object-table-create-info-nvx  1000086000)
  (indirect-commands-layout-create-info-nvx  1000086001)
  (cmd-process-commands-info-nvx  1000086002)
  (cmd-reserve-space-for-commands-info-nvx  1000086003)
  (device-generated-commands-limits-nvx  1000086004)
  (device-generated-commands-features-nvx  1000086005)
  (pipeline-viewport-w-scaling-state-create-info-nv  1000087000)
  (surface-capabilities-2-ext  1000090000)
  (display-power-info-ext  1000091000)
  (device-event-info-ext  1000091001)
  (display-event-info-ext  1000091002)
  (swapchain-counter-create-info-ext  1000091003)
  (present-times-info-google  1000092000)
  (physical-device-multiview-per-view-attributes-properties-nvx  1000097000)
  (pipeline-viewport-swizzle-state-create-info-nv  1000098000)
  (physical-device-discard-rectangle-properties-ext  1000099000)
  (pipeline-discard-rectangle-state-create-info-ext  1000099001)
  (physical-device-conservative-rasterization-properties-ext  1000101000)
  (pipeline-rasterization-conservative-state-create-info-ext  1000101001)
  (physical-device-depth-clip-enable-features-ext  1000102000)
  (pipeline-rasterization-depth-clip-state-create-info-ext  1000102001)
  (hdr-metadata-ext  1000105000)
  (physical-device-imageless-framebuffer-features-khr  1000108000)
  (framebuffer-attachments-create-info-khr  1000108001)
  (framebuffer-attachment-image-info-khr  1000108002)
  (render-pass-attachment-begin-info-khr  1000108003)
  (attachment-description-2-khr  1000109000)
  (attachment-reference-2-khr  1000109001)
  (subpass-description-2-khr  1000109002)
  (subpass-dependency-2-khr  1000109003)
  (render-pass-create-info-2-khr  1000109004)
  (subpass-begin-info-khr  1000109005)
  (subpass-end-info-khr  1000109006)
  (shared-present-surface-capabilities-khr  1000111000)
  (import-fence-win32-handle-info-khr  1000114000)
  (export-fence-win32-handle-info-khr  1000114001)
  (fence-get-win32-handle-info-khr  1000114002)
  (import-fence-fd-info-khr  1000115000)
  (fence-get-fd-info-khr  1000115001)
  (physical-device-performance-query-features-khr  1000116000)
  (physical-device-performance-query-properties-khr  1000116001)
  (query-pool-performance-create-info-khr  1000116002)
  (performance-query-submit-info-khr  1000116003)
  (acquire-profiling-lock-info-khr  1000116004)
  (performance-counter-khr  1000116005)
  (performance-counter-description-khr  1000116006)
  (physical-device-surface-info-2-khr  1000119000)
  (surface-capabilities-2-khr  1000119001)
  (surface-format-2-khr  1000119002)
  (display-properties-2-khr  1000121000)
  (display-plane-properties-2-khr  1000121001)
  (display-mode-properties-2-khr  1000121002)
  (display-plane-info-2-khr  1000121003)
  (display_plane-capabilities-2-khr  1000121004)
  (ios-surface-create-info-mvk  1000122000)
  (macos-surface-create-info-mvk  1000123000)
  (debug-utils-object-name-info-ext  1000128000)
  (debug-utils-object-tag-info-ext  1000128001)
  (debug-utils-label-ext  1000128002)
  (debug-utils-messenger-callback-data-ext  1000128003)
  (debug-utils-messenger-create-info-ext  1000128004)
  (android-hardware-buffer-usage-android  1000129000)
  (android-hardware-buffer-properties-android  1000129001)
  (android-hardware-buffer-format-properties-android  1000129002)
  (import-android-hardware-buffer-info-android  1000129003)
  (memory-get-android-hardware-buffer-info-android  1000129004)
  (external-format-android  1000129005)
  (physical-device-sampler-filter-minmax-properties-ext  1000130000)
  (sampler-reduction-mode-create-info-ext  1000130001)
  (physical-device-inline-uniform-block-features-ext  1000138000)
  (physical-device-inline-uniform-block-properties-ext  1000138001)
  (write-descriptor-set-inline-uniform-block-ext  1000138002)
  (descriptor-pool-inline-uniform-block-create-info-ext  1000138003)
  (sample-locations-info-ext  1000143000)
  (render-pass-sample-locations-begin-info-ext  1000143001)
  (pipeline-sample-locations-state-create-info-ext  1000143002)
  (physical-device-sample-locations-properties-ext  1000143003)
  (multisample-properties-ext  1000143004)
  (image-format-list-create-info-khr  1000147000)
  (physical-device-blend-operation-advanced-features-ext  1000148000)
  (physical-device-blend-operation-advanced-properties-ext  1000148001)
  (pipeline-color-blend-advanced-state-create-info-ext  1000148002)
  (pipeline-coverage-to-color-state-create-info-nv  1000149000)
  (pipeline-coverage-modulation-state-create-info-nv  1000152000)
  (physical-device-shader-sm-builtins-features-nv  1000154000)
  (physical-device-shader-sm-builtins-properties-nv  1000154001)
  (drm-format-modifier-properties-list-ext  1000158000)
  (drm-format-modifier-properties-ext  1000158001)
  (physical-device-image-drm-format-modifier-info-ext  1000158002)
  (image-drm-format-modifier-list-create-info-ext  1000158003)
  (image-drm-format-modifier-explicit-create-info-ext  1000158004)
  (image-drm-format-modifier-properties-ext  1000158005)
  (validation-cache-create-info-ext  1000160000)
  (shader-module-validation-cache-create-info-ext  1000160001)
  (descriptor-set-layout-binding-flags-create-info-ext  1000161000)
  (physical-device-descriptor-indexing-features-ext  1000161001)
  (physical-device-descriptor-indexing-properties-ext  1000161002)
  (descriptor-set-variable-descriptor-count-allocate-info-ext  1000161003)
  (descriptor-set-variable-descriptor-count-layout-support-ext  1000161004)
  (pipeline-viewport-shading-rate-image-state-create-info-nv  1000164000)
  (physical-device-shading-rate-image-features-nv  1000164001)
  (physical-device-shading-rate-image-properties-nv  1000164002)
  (pipeline-viewport-coarse-sample-order-state-create-info-nv  1000164005)
  (ray-tracing-pipeline-create-info-nv  1000165000)
  (acceleration-structure-create-info-nv  1000165001)
  (geometry-nv  1000165003)
  (geometry-triangles-nv  1000165004)
  (geometry-aabb-nv  1000165005)
  (bind-acceleration-structure-memory-info-nv  1000165006)
  (write-descriptor-set-acceleration-structure-nv  1000165007)
  (acceleration-structure-memory-requirements-info-nv  1000165008)
  (physical-device-ray-tracing-properties-nv  1000165009)
  (ray-tracing-shader-group-create-info-nv  1000165011)
  (acceleration-structure-info-nv  1000165012)
  (physical-device-representative-fragment-test-features-nv  1000166000)
  (pipeline-representative-fragment-test-state-create-info-nv  1000166001)
  (physical-device-image-view-image-format-info-ext  1000170000)
  (filter-cubic-image-view-image-format-properties-ext  1000170001)
  (device-queue-global-priority-create-info-ext  1000174000)
  (physical-device-shader-subgroup-extended-types-features-khr  1000175000)
  (physical-device-8bit-storage-features-khr  1000177000)
  (import-memory-host-pointer-info-ext  1000178000)
  (memory-host-pointer-properties-ext  1000178001)
  (physical-device-external-memory-host-properties-ext  1000178002)
  (physical-device-shader-atomic-int64-features-khr  1000180000)
  (physical-device-shader-clock-features-khr  1000181000)
  (pipeline-compiler-control-create-info-amd  1000183000)
  (calibrated-timestamp-info-ext  1000184000)
  (physical-device-shader-core-properties-amd  1000185000)
  (device-memory-overallocation-create-info-amd  1000189000)
  (physical-device-vertex-attribute-divisor-properties-ext  1000190000)
  (pipeline-vertex-input-divisor-state-create-info-ext  1000190001)
  (physical-device-vertex-attribute-divisor-features-ext  1000190002)
  (present-frame-token-ggp  1000191000)
  (pipeline-creation-feedback-create-info-ext  1000192000)
  (physical-device-driver-properties-khr  1000196000)
  (physical-device-float-controls-properties-khr  1000197000)
  (physical-device-depth-stencil-resolve-properties-khr  1000199000)
  (subpass-description-depth-stencil-resolve-khr  1000199001)
  (physical-device-compute-shader-derivatives-features-nv  1000201000)
  (physical-device-mesh-shader-features-nv  1000202000)
  (physical-device-mesh-shader-properties-nv  1000202001)
  (physical-device-fragment-shader-barycentric-features-nv  1000203000)
  (physical-device-shader-image-footprint-features-nv  1000204000)
  (pipeline-viewport-exclusive-scissor-state-create-info-nv  1000205000)
  (physical-device-exclusive-scissor-features-nv  1000205002)
  (checkpoint-data-nv  1000206000)
  (queue-family-checkpoint-properties-nv  1000206001)
  (physical-device-timeline-semaphore-features-khr  1000207000)
  (physical-device-timeline-semaphore-properties-khr  1000207001)
  (semaphore-type-create-info-khr  1000207002)
  (timeline-semaphore-submit-info-khr  1000207003)
  (semaphore-wait-info-khr  1000207004)
  (semaphore-signal-info-khr  1000207005)
  (physical-device-shader-integer-functions-2-features-intel  1000209000)
  (query-pool-create-info-intel  1000210000)
  (initialize-performance-api-info-intel  1000210001)
  (performance-marker-info-intel  1000210002)
  (performance-stream-marker-info-intel  1000210003)
  (performance-override-info-intel  1000210004)
  (performance-configuration-acquire-info-intel  1000210005)
  (physical-device-vulkan-memory-model-features-khr  1000211000)
  (physical-device-pci-bus-info-properties-ext  1000212000)
  (display-native-hdr-surface-capabilities-amd  1000213000)
  (swapchain-display-native-hdr-create-info-amd  1000213001)
  (imagepipe-surface-create-info-fuchsia  1000214000)
  (metal-surface-create-info-ext  1000217000)
  (physical-device-fragment-density-map-features-ext  1000218000)
  (physical-device-fragment-density-map-properties-ext  1000218001)
  (render-pass-fragment-density-map-create-info-ext  1000218002)
  (physical-device-scalar-block-layout-features-ext  1000221000)
  (physical-device-subgroup-size-control-properties-ext  1000225000)
  (pipeline-shader-stage-required-subgroup-size-create-info-ext  1000225001)
  (physical-device-subgroup-size-control-features-ext  1000225002)
  (physical-device-shader-core-properties-2-amd  1000227000)
  (physical-device-coherent-memory-features-amd  1000229000)
  (physical-device-memory-budget-properties-ext  1000237000)
  (physical-device-memory-priority-features-ext  1000238000)
  (memory-priority-allocate-info-ext  1000238001)
  (surface-protected-capabilities-khr  1000239000)
  (physical-device-dedicated-allocation-image-aliasing-features-nv  1000240000)
  (physical-device-separate-depth-stencil-layouts-features-khr  1000241000)
  (attachment-reference-stencil-layout-khr  1000241001)
  (attachment-description-stencil-layout-khr  1000241002)
  (physical-device-buffer-device-address-features-ext  1000244000)
  (buffer-device-address-create-info-ext  1000244002)
  (physical-device-tool-properties-ext  1000245000)
  (image-stencil-usage-create-info-ext  1000246000)
  (validation-features-ext  1000247000)
  (physical-device-cooperative-matrix-features-nv  1000249000)
  (cooperative-matrix-properties-nv  1000249001)
  (physical-device-cooperative-matrix-properties-nv  1000249002)
  (physical-device-coverage-reduction-mode-features-nv  1000250000)
  (pipeline-coverage-reduction-state-create-info-nv  1000250001)
  (framebuffer-mixed-samples-combination-nv  1000250002)
  (physical-device-fragment-shader-interlock-features-ext  1000251000)
  (physical-device-ycbcr-image-arrays-features-ext  1000252000)
  (physical-device-uniform-buffer-standard-layout-features-khr  1000253000)
  (surface-full-screen-exclusive-info-ext  1000255000)
  (surface-capabilities-full-screen-exclusive-ext  1000255002)
  (surface-full-screen-exclusive-win32-info-ext  1000255001)
  (headless-surface-create-info-ext  1000256000)
  (physical-device-buffer-device-address-features-khr  1000257000)
  (buffer-device-address-info-khr  1000244001)
  (buffer-opaque-capture-address-create-info-khr  1000257002)
  (memory-opaque-capture-address-allocate-info-khr  1000257003)
  (device-memory-opaque-capture-address-info-khr  1000257004)
  (physical-device-line-rasterization-features-ext  1000259000)
  (pipeline-rasterization-line-state-create-info-ext  1000259001)
  (physical-device-line-rasterization-properties-ext  1000259002)
  (physical-device-host-query-reset-features-ext  1000261000)
  (physical-device-index-type-uint8-features-ext  1000265000)
  (physical-device-pipeline-executable-properties-features-khr  1000269000)
  (pipeline-info-khr  1000269001)
  (pipeline-executable-properties-khr  1000269002)
  (pipeline-executable-info-khr  1000269003)
  (pipeline-executable-statistic-khr  1000269004)
  (pipeline-executable-internal-representation-khr  1000269005)
  (physical-device-shader-demote-to-helper-invocation-features-ext  1000276000)
  (physical-device-texel-buffer-alignment-features-ext  1000281000)
  (physical-device-texel-buffer-alignment-properties-ext  1000281001))
