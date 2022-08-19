(load "image.scm")
(load "vec3.scm")
(load "geometry.scm")


(define pi 3.14159265359)

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
          t1
          point
          normal
          material))

(define-record-type material
  (fields roughness
          metalness
          color
          ; draw might be null, use if not and pass self and hit-info
          debug-draw))

; SCENE
(define camera
  (make-camera (make-vec3 0. 0. 0.)
               (make-vec3 0. 0. -1.)
               1.0
               40.0))

(define (debug-draw-normal material hit)
  (if (hit-info-hit? hit)
    (hit-info-normal hit)
    (make-vec3 0. 0. 0.)))

(define null-material
  (make-material 
    1.0 0. (make-vec3 0. 0. 0.) (list)))

(define null-hit-info
  (make-hit-info
    #f (list) (list) (list) (list) (list)))

(define temp-material 
  (make-material
    1.
    0.
    (make-vec3 0.2 0.6 0.2)
    debug-draw-normal))

(define scene
  (list
    (make-sphere
      (make-vec3 0.5 -0.5 -4.9)
      0.5
      temp-material)
    (make-sphere
      (make-vec3 -1. 0.5 -6.1)
      0.5
      temp-material)
    (make-sphere
      (make-vec3 -0.2 0. -5.5)
      0.5
      temp-material)))

(define imagebuffer (make-image 768 432))

(define (brdf-render material hit-info)
  (make-vec3 1.0 0.0 1.0))

(define (draw-material material hit-info)
  (if (not (null? (material-debug-draw material)))
    ((material-debug-draw material) material hit-info)
    (brdf-render material hit-info)))

; DERIVE
(define width (image-width imagebuffer))
(define height (image-height imagebuffer))
(define num-pixels (* width height))
(define aspect-ratio (/ width height))

(define (luminance-in ray-start ray-dir scene)
  (define (sphere-hit sphere)
    (let ([hit (sphere-intersect sphere (make-ray ray-start ray-dir))])
      (if (car hit)
        (let* ([hit-point (vec-muls (vec-add ray-start ray-dir) (cadr hit))]
               [hit-normal (vec-normalized (vec-sub hit-point (sphere-center sphere)))])
          (make-hit-info (car hit) (cadr hit) (caddr hit) hit-point hit-normal (sphere-material sphere)))
        null-hit-info)))
  (define (gather-hits scene)
    (filter (lambda (h) (hit-info-hit? h)) (map sphere-hit scene)))
  (let ([hits (list-sort (lambda (a b) (< (hit-info-t0 a) (hit-info-t0 b))) (gather-hits scene))])
    (if (null? hits)
      (make-vec3 0.1 0.1 0.1)
      (draw-material (hit-info-material (car hits)) (car hits)))))

 
(define (get-color x y)
  (let* ([fov-adjust (tan (* (camera-fov camera) 0.5 (/ pi 180.)))]
         [in-pixel-x (+ x 0.5)]
         [in-pixel-y (+ y 0.5)]
         [px (* (- (* 2.0 (/ in-pixel-x width )) 1.0 ) fov-adjust aspect-ratio)]
         [py (* (- 1.0 (* 2.0 (/ in-pixel-y height))) fov-adjust)]
         [ray-dir (vec-normalized (vec-sub (make-vec3 px py (vec-z (camera-direction camera))) (camera-position camera)))])
    (begin
      (luminance-in (camera-position camera) ray-dir scene))))


(define (render-to-buffer imagebuffer)
  (let ([width (image-width imagebuffer)] [height (image-height imagebuffer)])
    (define (render-rows imagebuffer)
      (define (render-row row y)
        (define (render-pixel row x y)
          (if (< x width)
            (list-set! row 0 (float-list-to-u8 (get-color x y)))))
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
