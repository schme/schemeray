
(library (vec3)
         (export make-vec-from-points
                 vec-x vec-y vec-z vec-w
                 vec-divs vec-muls
                 vec-add vec-sub vec-mul vec-div
                 vec-length2 vec-length vec-normalized vec-dot vec-dot-self
                 vec-lerp vec-reflect

                 make-vec3 make-vec3-zero 
                 vec3-divs vec3-muls
                 vec3-add vec3-sub vec3-mul vec3-div
                 vec3-length2 vec3-length vec3-normalized vec3-dot vec3-dot-self vec3-cross
                 vec3-lerp vec3-reflect)
         (import (rnrs))

(define (lerp a b t)
  (+ (* a (- 1 t)) (* b t)))


; Construct

(define (make-vec3 x y z)
  (list x y z))

(define (make-vec3-zero)
  (list 0. 0. 0.))

(define (make-vec-from-points p1 p2)
  (map (lambda (a b) (- b a)) p1 p2))


; Access

(define (vec-x vec)
  (car vec))

(define (vec-y vec)
  (cadr vec))

(define (vec-z vec)
  (caddr vec))

(define (vec-w vec)
  (cadddr vec))


; Operators

(define (vec-divs vec val)
  (map (lambda (a) (/ a val)) vec))

(define (vec-muls vec val)
  (map (lambda (a) (* a val)) vec))

(define (vec-add vec-a vec-b)
  (map (lambda (a b) (+ a b)) vec-a vec-b))

(define (vec-sub vec-a vec-b)
  (map (lambda (a b) (- a b)) vec-a vec-b))

(define (vec-mul vec-a vec-b)
  (map (lambda (a b) (* a b)) vec-a vec-b))

(define (vec-div vec-a vec-b)
  (map (lambda (a b) (/ a b)) vec-a vec-b))

; vec3 specific, unrolled for better performance

(define (vec3-divs vec val)
  (list (/ (vec-x vec) val)
        (/ (vec-y vec) val)
        (/ (vec-z vec) val)))

(define (vec3-muls vec val)
  (list (* (vec-x vec) val)
        (* (vec-y vec) val)
        (* (vec-z vec) val)))

(define (vec3-add vec-a vec-b)
  (list (+ (vec-x vec-a) (vec-x vec-b))
        (+ (vec-y vec-a) (vec-y vec-b)) 
        (+ (vec-z vec-a) (vec-z vec-b))))

(define (vec3-sub vec-a vec-b)
  (list (- (vec-x vec-a) (vec-x vec-b))
        (- (vec-y vec-a) (vec-y vec-b)) 
        (- (vec-z vec-a) (vec-z vec-b))))

(define (vec3-mul vec-a vec-b)
  (list (* (vec-x vec-a) (vec-x vec-b))
        (* (vec-y vec-a) (vec-y vec-b)) 
        (* (vec-z vec-a) (vec-z vec-b))))

(define (vec3-div vec-a vec-b)
  (list (/ (vec-x vec-a) (vec-x vec-b))
        (/ (vec-y vec-a) (vec-y vec-b)) 
        (/ (vec-z vec-a) (vec-z vec-b))))

; Operations

(define (vec-length2 vec)
  (fold-left (lambda (acc a) (+ (* a a) acc)) 0 vec))

(define (vec-length vec)
  (sqrt (vec-length2 vec)))

(define (vec-normalized vec)
  (vec-divs vec (vec-length vec)))

(define (vec-dot vec-a vec-b)
  (fold-left + 0 (map (lambda (a b) (* a b)) vec-a vec-b)))

(define (vec-dot-self vec)
  (vec-dot vec vec))

; Vec3 

(define (vec3-length2 vec)
  (let ([x (vec-x vec)]
        [y (vec-y vec)]
        [z (vec-z vec)])
    (+ (* x x)
       (* y y)
       (* z z))))

(define (vec3-length vec)
  (sqrt (vec3-length2 vec)))

(define (vec3-normalized vec)
  (vec3-divs vec (vec3-length vec)))

; unroll for efficiency
(define (vec3-dot vec-a vec-b)
  (let ([ax (car vec-a)]
        [ay (cadr vec-a)]
        [az (caddr vec-a)]
        [bx (car vec-b)]
        [by (cadr vec-b)]
        [bz (caddr vec-b)])
    (+ (* ax bx)
       (* ay by)
       (* az bz))))

(define (vec3-dot-self vec)
  (vec3-dot vec vec))

(define (vec3-cross a b)
  (make-vec3 (- (* (vec-y a) (vec-z b)) (* (vec-z a) (vec-y b)))
             (- (* (vec-z a) (vec-x b)) (* (vec-x a) (vec-z b)))
             (- (* (vec-x a) (vec-y b)) (* (vec-y a) (vec-x b)))))

; Utility

; TODO: check correctness
(define (vec-lerp vec-a vec-b t)
  (map (lambda (a b) (lerp a b t)) vec-a vec-b ))

; TODO: check correctness
; r = v - 2(v . n)n
(define (vec-reflect vec normal)
  (vec-sub vec (vec-muls normal (* 2 (vec-dot vec normal)))))

; vec3

; TODO: check correctness
(define (vec3-lerp vec-a vec-b t)
  (list (lerp (vec-x vec-a) (vec-x vec-b) t)
        (lerp (vec-y vec-a) (vec-y vec-b) t)
        (lerp (vec-z vec-a) (vec-z vec-b) t)))

; TODO: check correctness
; r = v - 2(v . n)n
(define (vec3-reflect vec normal)
  (vec3-sub vec (vec3-muls normal (* 2 (vec3-dot vec normal)))))) 


