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

(define-record-type camera
  (fields position
          direction
          nearplane
          fov))


(define (list-add a b)
  (map + a b))

; SCENE
(define camera
  (make-camera (make-vec3 0. 0. 0.)
               (make-vec3 0. 0. -1.)
               1.0
               40.0))

(define scene
  (list
    (make-sphere (make-vec3 0. 0. -5.)
               0.5)
    (make-sphere (make-vec3 -1. 1. -6.)
                 1.0)))

(define imagebuffer (make-image 768 432))

; DERIVE
(define width (image-width imagebuffer))
(define height (image-height imagebuffer))
(define num-pixels (* width height))
(define aspect-ratio (/ width height))


(define (luminance-in ray-start ray-dir)
  (define (sphere-hit sphere)
    (let ([hit (sphere-intersect sphere (make-ray ray-start ray-dir))])
      (if (car hit)
        (let* ([hit-point (vec-muls (vec-add ray-start ray-dir) (cadr hit) )]
               [hit-normal (vec-normalized (vec-sub hit-point (sphere-center scene)))])
          (vector->list hit-normal))
        (list 0.25 0.25 0.25))))
  ())


(define (get-color x y)
  (let* ([fov-adjust (tan (* (camera-fov camera) 0.5 (/ pi 180.)))]
         [in-pixel-x (+ x 0.5)]
         [in-pixel-y (+ y 0.5)]
         [px (* (- (* 2.0 (/ in-pixel-x width )) 1.0 ) fov-adjust aspect-ratio)]
         [py (* (- 1.0 (* 2.0 (/ in-pixel-y height))) fov-adjust)]
         [ray-dir (vec-normalized (vec-sub (make-vec3 px py (vec-z (camera-direction camera))) (camera-position camera)))])
    (luminance-in (camera-position camera) ray-dir)))


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
