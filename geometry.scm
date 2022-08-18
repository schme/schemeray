(load "vec3.scm")

(define-record-type ray 
  (fields origin
          direction))

(define-record-type sphere 
  (fields center
          radius2
          material))

(define (sphere-intersect sphere ray)
  (define (discriminant a b c)
    (sqrt (- (* b b)
             (* 4.0 a c))))
  (let* ([L (vec-sub (ray-origin ray) (sphere-center sphere))]
         [a (vec-dot-self (ray-direction ray))]
         [b (* 2.0 (vec-dot (ray-direction ray) L))]
         [c (- (vec-dot L L) (sphere-radius2 sphere))]
         [disc (discriminant a b c)]
         [hit (and (real-valued? disc) (>= disc 0))])
    (cond ((not hit) (list hit))
          ((= b 0) (list hit (* -0.5 (/ b a)) (* -0.5 (/ b a))))
          (else
            (let* ([q
                   (cond ((< b 0) (* -0.5 (- b (sqrt disc))))
                         ((> b 0) (* -0.5 (+ b (sqrt disc)))))]
                  [t0 (/ q a)]
                  [t1 (/ c q)])
              (cond ((and (< t0 0) (< t1 0)) (list #f))
                    ((< t0 0) (list hit t1 t1))
                    (else (list hit t0 t1))))))))
