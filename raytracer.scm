(library (raytracer)
         (export run-trace write-buffer trace-and-write)
         (import (chezscheme)
                 (threads)
                 (vec3)
                 (geometry)
                 (image))

(define pi 3.14159265359)
(define hit-epsilon 0.001)

(define (float-to-u8 v)
  (let ([out (* (norm v) 255)])
    (if (flonum? out)
      (flonum->fixnum out)
      out)))

(define (float-list-to-u8 l)
  (map (lambda (v) (float-to-u8 v)) l))

(define (norm val)
  (clamp 0.0 1.0 val))

(define (clamp edge0 edge1 val)
  (cond
    ((<= val edge0) edge0)
    ((>= val edge1) edge1)
    (else val)))

; SOME TYPES

(define-record-type camera
  (fields position
          direction
          nearplane
          fov))


(define-record-type material
  (fields roughness
          metalness
          diffuse
          emissive))

; Debug

(define (debug-draw-normal material hit)
  (if (not (null? hit))
    (hit-info-normal hit)
    (make-vec3 0. 0. 0.)))

(define (debug-draw-normal-false material hit)
  (if (not (null? hit))
    (vec3-muls (vec3-add (hit-info-normal hit) (make-vec3 1. 1. 1)) 0.5)
    (make-vec3 0. 0. 0.)))

; Tune constants to the scene
(define (debug-draw-distance material hit)
  (let ([out-color
          (vec3-sub (make-vec3 1. 1. 1)
                   (vec3-muls (make-vec3 1. 1. 1.) (* 0.1 (hit-info-t0 hit))))])
    out-color))

; SCENE
(define scene-cornellish-balls
  (list
    (make-sphere
      (make-vec3 -1.8 0.0 -8.0)
      1.0
      (make-material 1. 0. (make-vec3 1. 1. 1.) (make-vec3-zero)))
    (make-sphere
      (make-vec3 0. -1.0 -8.0)
      1.0
      (make-material 1. 0. (make-vec3 1. 1. 1.) (make-vec3-zero)))
    (make-sphere
      (make-vec3 1.8 0.0 -8.0)
      1.0
      (make-material 1. 0. (make-vec3 1. 1. 1.) (make-vec3-zero)))
    (make-sphere
      (make-vec3 0. 0. -50)
      40.5
      (make-material 0. 0. (make-vec3 1.0 0.0 0.0) (make-vec3-zero)))
    (make-sphere
      (make-vec3 50. 0. -10)
      47
      (make-material 0. 0. (make-vec3 1.0 0.0 1.0) (make-vec3-zero)))
    (make-sphere
      (make-vec3 -50. 0. -10)
      47
      (make-material 0. 0. (make-vec3 0.0 1.0 1.0) (make-vec3-zero)))
    (make-sphere
      (make-vec3 0. -50. -10)
      48
      (make-material 0. 0. (make-vec3 0.0 1.0 0.0) (make-vec3-zero)))
    (make-sphere
      (make-vec3 0. 50.0 -8.)
      47
      (make-material 0. 0. (make-vec3-zero) (make-vec3 1.0 1.0 1.0)))))

(define scene-cornellish-balls-camera
  (make-camera (make-vec3 0. 0. 0.)
               (make-vec3 0. 0. -1.)
               1.0
               45))

(define scene scene-cornellish-balls)
(define scene-camera scene-cornellish-balls-camera)

(define samples-per-pixel (* 1024 1))
(define maximum-depth 5)
(define ambient-color (make-vec3 0.0 0.0 0.0))

(define debug-function '())


;(define imagebuffer (make-image 768 432))
;(define imagebuffer (make-image 256 256))
;(define imagebuffer (make-image 128 128))
(define imagebuffer (make-image 64 64))
;(define imagebuffer (make-image 32 32))


; DERIVED
(define width (image-width imagebuffer))
(define height (image-height imagebuffer))
(define aspect-ratio (/ width height))

(define (uniform-sample-hemisphere normal)
  (let* ([u (random 1.0)]
         [v (random 1.0)]
         [theta (* 2.0 pi u)]
         [phi (acos (- (* 2.0 v) 1.0))]
         [dirx (* (cos theta) (sin phi))]
         [diry (* (sin theta) (sin phi))]
         [dirz (cos phi)]
         [dir (vec3-normalized (make-vec3 dirx diry dirz))]
         [dirdotn (vec3-dot dir normal)])
    (if (> 0. dirdotn)
      (vec3-muls dir -1)
      dir)))

(define (gather-hits ray-start ray-dir scene)
  (let ([all-hits
          (map (lambda (s) (sphere-intersect s (make-ray ray-start ray-dir))) scene)])
    (list-sort (lambda (a b) (< (hit-info-t0 a) (hit-info-t0 b)))
               (filter (lambda (h) (not (null? h))) all-hits))))

(define (radiance-out hit ray-dir depth)
  (let* ([new-ray-dir (uniform-sample-hemisphere (hit-info-normal hit))]
         [material (hit-info-material hit)]
         [new-hits (gather-hits
                     (vec3-add
                       (hit-info-point hit)
                       (vec3-muls new-ray-dir hit-epsilon)) new-ray-dir scene)]
         [new-dir-dot-n-2 (* 2.0 (vec3-dot new-ray-dir (hit-info-normal hit)))]
         [L0 (material-emissive material)])
    (if (or (= 0 depth) (null? new-hits))
      (vec3-add
        L0
        (vec3-muls
          (vec3-mul ambient-color (material-diffuse material))
          new-dir-dot-n-2))
      (vec3-add
        L0
        (vec3-mul
          (vec3-muls (material-diffuse material) new-dir-dot-n-2)
          (radiance-out (car new-hits) new-ray-dir (- depth 1)))))))

(define (radiance-in ray-start ray-dir scene)
  (let ([hits (gather-hits ray-start ray-dir scene)])
     (if (null? hits)
        (make-vec3-zero)
        (radiance-out (car hits) ray-dir maximum-depth))))

(define (get-debug-color-at-direction ray-start ray-dir scene)
   (let ([hits (gather-hits ray-start ray-dir scene)])
     (if (not (null? hits))
      (debug-function (hit-info-material (car hits)) (car hits)))))

(define (get-color x y samples)
  (define (get-color-at-direction ray-start ray-dir scene)
    (let loop ([i 0]
               [color (make-vec3-zero)])
      (if (> i samples) (vec3-divs color samples)
        (loop (+ i 1)
              (vec3-add
                  color
                  (radiance-in ray-start ray-dir scene))))))
  (let* ([render-func (if (null? debug-function) get-color-at-direction get-debug-color-at-direction)]
         [fov-adjust (tan (* (camera-fov scene-camera) 0.5 (/ pi 180.)))]
         [in-pixel-x (+ x 0.5)]
         [in-pixel-y (+ y 0.5)]
         [px (* (- (* 2.0 (/ in-pixel-x width )) 1.0 ) fov-adjust aspect-ratio)]
         [py (* (- 1.0 (* 2.0 (/ in-pixel-y height))) fov-adjust)]
         [ray-dir (vec3-normalized
                    (vec3-sub
                      (make-vec3 px py (vec-z (camera-direction scene-camera)))
                      (camera-position scene-camera)))])
    (render-func (camera-position scene-camera) ray-dir scene)))


(define (render-to-buffer imagebuffer width height pixel)
  (let ([y (image-get-y imagebuffer pixel)]
        [x (image-get-x imagebuffer pixel)])
    (image-set! imagebuffer x y (float-list-to-u8 (get-color x y samples-per-pixel)))))

(define current-pixel 0)
(define (render-thread)
  (let ([width (image-width imagebuffer)]
        [height (image-height imagebuffer)]
        [pixels (* width height)])
    (if (< current-pixel pixels)
      (let ([pxl current-pixel])
       (begin
        (set! current-pixel (+ current-pixel 1))
        (render-to-buffer imagebuffer width height pxl)
        (render-thread))))))

(define (run-trace)
  (random-seed 1337)
  (if (not (null? debug-function))
    (begin
      (set! samples-per-pixel 1)
      (set! maximum-depth 1)))
  (time
    (let ([threads (start-threads render-thread 8)])
      (wait-threads threads))))

(define (write-buffer filename)
  (display "\nFile: ")
  (display filename)
  (write-ppm imagebuffer filename)
  (set! current-pixel 0)
  (display ". Done.")
  (newline))

(define (trace-and-write filename)
  (run-trace)
  (write-buffer filename))

(define (trace-and-write-with-upscale filename)
  (let ([file (format "~s.ppm" filename)])
    (trace-and-write file)
    (system (format "convert -scale 1000% ~s.ppm ~s_large.ppm" filename filename))))

)


