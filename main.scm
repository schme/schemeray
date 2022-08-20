(load "image.scm")
(load "vec3.scm")
(load "geometry.scm")


(define pi 3.14159265359)
(define hit-epsilon 0.001)
(random-seed 8008135)

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

(define (list-add a b)
  (map + a b))

; SOME TYPES

(define-record-type camera
  (fields position
          direction
          nearplane
          fov))


(define-record-type hit-info
  (fields hit?
          t0
          point
          normal
          material))

(define-record-type material
  (fields roughness
          metalness
          diffuse
          emissive))

; SCENE
(define camera
  (make-camera (make-vec3 0. 0. 0.)
               (make-vec3 0. 0. -1.)
               1.0
               40.0))

(define (debug-draw-normal material hit)
  (if (not (null? hit))
    (hit-info-normal hit)
    (make-vec3 0. 0. 0.)))

; Tune constants to the scene
(define (debug-draw-distance material hit)
  (let ([out-color (vec-sub (make-vec3 1. 1. 1) (vec-muls (make-vec3 1. 1. 1.) (* 0.1 (hit-info-t0 hit))))])
    out-color))

(define temp-material 
  (make-material
    1.
    0.
    (make-vec3 1.0 1.0 1.0)
    (make-vec3 0. 0. 0.)))

(define scene
  (list
    (make-sphere
      (make-vec3 -1.8 0.0 -8.0)
      1.0
      temp-material)
    (make-sphere
      (make-vec3 0. -1.0 -8.0)
      1.0
      temp-material)
    (make-sphere
      (make-vec3 1.8 0.0 -8.0)
      1.0
      temp-material)
    (make-sphere
      (make-vec3 0. 0. -50)
      40
      (make-material 0. 0. (make-vec3 1.0 0.0 0.0) (make-vec3-zero)))
    (make-sphere
      (make-vec3 50. 0. -10)
      47
      (make-material 0. 0. (make-vec3 0.0 0.0 1.0) (make-vec3-zero)))
    (make-sphere
      (make-vec3 -50. 0. -10)
      47
      (make-material 0. 0. (make-vec3 0.0 0.0 1.0) (make-vec3-zero)))
    (make-sphere
      (make-vec3 0. -50. -10)
      48
      (make-material 0. 0. (make-vec3 0.0 1.0 0.0) (make-vec3-zero)))
    (make-sphere
      (make-vec3 0. 12.0 -5.)
      10
      (make-material 0. 0. (make-vec3-zero) (make-vec3 1. 1. 1.)))))

(define samples-per-pixel 1000)
(define maximum-depth 5)
(define ambient-color (make-vec3 0.5 0.5 0.5))

(define debug-function '())

; So I don't have to remember to set these
(if (not (null? debug-function))
  (begin
    (set! samples-per-pixel 1)
    (set! maximum-depth 1)))

;(define imagebuffer (make-image 768 432))
(define imagebuffer (make-image 64 64))

; DERIVE
(define width (image-width imagebuffer))
(define height (image-height imagebuffer))
(define num-pixels (* width height))
(define aspect-ratio (/ width height))

(define (uniform-sample-hemisphere normal)
  (let* ([u (random 1.0)]
        [v (random 1.0)]
        [theta (* 2.0 pi u)]
        [phi (acos (- (* 2.0 v) 1.0))]
        [dirx (* (cos theta) (sin phi))]
        [diry (* (sin theta) (sin phi))]
        [dirz (cos phi)]
        [dir (vec-normalized (make-vec3 dirx diry dirz))]
        [dirdotn (vec-dot dir normal)])
    (if (<= 0. dirdotn)
      (vec-muls dir -1)
      dir)))

(define (gather-hits ray-start ray-dir scene)
  (filter
    (lambda (h) (and (not (null? h)) (hit-info-hit? h)))
    (map (lambda (s) (sphere-intersect s (make-ray ray-start ray-dir))) scene)))

(define (luminance-out hit ray-dir depth)
  (if (= 0 depth)
    (make-vec3-zero)
    (let* ([new-ray-dir (uniform-sample-hemisphere (hit-info-normal hit))]
           [material (hit-info-material hit)]
           [hits (gather-hits (vec-add (hit-info-point hit) (vec-muls new-ray-dir hit-epsilon)) new-ray-dir scene)])
      (if (null? hits)
        (vec-add
          (material-emissive (hit-info-material hit))
          (vec-muls
            (vec-mul ambient-color (material-diffuse material))
            (* 2.0 (vec-dot new-ray-dir (hit-info-normal hit)))))
        (vec-add
          (material-emissive (hit-info-material hit))
          (vec-mul
            (vec-muls
              (material-diffuse material)
              (* 2.0 (vec-dot new-ray-dir (hit-info-normal hit))))
            (luminance-out (car hits) (vec-muls new-ray-dir -1) (- depth 1))))))))

(define (luminance-in ray-start ray-dir scene)
  (let ([hits (list-sort (lambda (a b) (< (hit-info-t0 a) (hit-info-t0 b))) (gather-hits ray-start ray-dir scene))])
    (if (and (not (null? hits)) (not (null? debug-function)))
      (debug-function (hit-info-material (car hits)) (car hits))
      (if (null? hits)
        (make-vec3-zero)
        (luminance-out (car hits) ray-dir maximum-depth)))))


(define (get-color x y sample)
  (define (get-color-sample x y)
    (let* ([fov-adjust (tan (* (camera-fov camera) 0.5 (/ pi 180.)))]
           [in-pixel-x (+ x 0.5)]
           [in-pixel-y (+ y 0.5)]
           [px (* (- (* 2.0 (/ in-pixel-x width )) 1.0 ) fov-adjust aspect-ratio)]
           [py (* (- 1.0 (* 2.0 (/ in-pixel-y height))) fov-adjust)]
           [ray-dir (vec-normalized (vec-sub (make-vec3 px py (vec-z (camera-direction camera))) (camera-position camera)))])
      (luminance-in (camera-position camera) ray-dir scene)))
  (if (= samples-per-pixel sample)
    (make-vec3-zero)
    (vec-add (vec-divs (get-color-sample x y) samples-per-pixel) (get-color x y (+ sample 1)))))

(define (render-to-buffer imagebuffer)
  (let ([width (image-width imagebuffer)] [height (image-height imagebuffer)])
    (define (render-rows imagebuffer)
      (define (render-row row y)
        (define (render-pixel row x y)
          (if (< x width)
            (list-set! row 0 (float-list-to-u8 (get-color x y 0)))))
        (if (not (null? row))
          (begin
            (render-pixel row (- width (length row)) y)
            (render-row (cdr row) y))))
      (if (not (null? imagebuffer))
        (begin 
          (render-row (car imagebuffer) (- height (image-height imagebuffer)))
          (render-rows (cdr imagebuffer)))))
    (render-rows imagebuffer)))

(render-to-buffer imagebuffer)

(let ([filename "render.ppm"])
    (display "File: ")
    (display filename)
    (write-ppm imagebuffer filename)
    (display ". Done.")
    (newline))
