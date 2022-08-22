(library (geometry)
 (export ray make-ray ray-origin ray-direction
         hit-info hit-info-hit? hit-info-t0 hit-info-point hit-info-normal hit-info-material
         sphere make-sphere sphere-center sphere-radius sphere-material
         sphere-intersect)
 (import (rnrs)
         (vec3))

 (define-record-type ray
   (fields origin
           direction))

(define-record-type hit-info
  (fields hit?
          t0
          point
          normal
          material))

 (define-record-type sphere
   (fields center
           radius
           material))

 (define (sphere-intersect sphere ray)
   (let* ([L (vec-sub (ray-origin ray) (sphere-center sphere))]
          [b (* (vec-dot L (ray-direction ray)))]
          [c (- (vec-dot L L) (* (sphere-radius sphere) (sphere-radius sphere)))]
          [disc (- (* b b) c)]
          [hit (> disc 0)])
     (if (not hit)
       '()
       (let* ([t0 (- (* -1 b) (sqrt disc))]
              [distance (if (< t0 0) (+ (* -1 b) (sqrt disc)) t0)]
              [get-normal
                (vec-normalized
                  (vec-sub (vec-add (ray-origin ray) (vec-muls (ray-direction ray) distance))
                           (sphere-center sphere)))]
              [normal
                (if (> 0(vec-dot get-normal (ray-direction ray)))
                  (vec-muls get-normal -1)
                  get-normal)]
              [hit-point (vec-add (ray-origin ray) (vec-muls (ray-direction ray) distance))])
         (make-hit-info #t distance hit-point normal (sphere-material sphere)))))))
