; vec3

(define (lerp a b t)
  (+ (* a (- 1 t)) (* b t)))

; Construct

(define (make-vec3 x y z)
  (vector x y z))

(define (make-vec-from-points p1 p2)
  (vector-map (lambda (a b) (- b a)) p1 p2))

; Access

(define (vec-x vec)
  (vector-ref vec 0))

(define (vec-y vec)
  (vector-ref vec 1))

(define (vec-z vec)
  (vector-ref vec 2))

; Modify

(define (vec-x-set! vec v)
  (vector-set! vec 0 v))

(define (vec-y-set! vec v)
  (vector-set! vec 1 v))

(define (vec-z-set! vec v)
  (vector-set! vec 2 v))

; Operators

(define (vec-divs vec-a val)
  (vector-map (lambda (a) (/ a val)) vec-a))

(define (vec-muls vec-a val)
  (vector-map (lambda (a) (* a val)) vec-a))

(define (vec-add vec-a vec-b)
  (vector-map (lambda (a b) (+ a b)) vec-a vec-b))

(define (vec-sub vec-a vec-b)
  (vector-map (lambda (a b) (- a b)) vec-a vec-b))


; Derive

(define (vec-length2 vec)
  (let ([acc 0])
    (vector-for-each (lambda (n) (set! acc (+ acc (expt n 2)))) vec)
    acc))

(define (vec-length vec)
  (sqrt (vec-length2 vec)))

(define (vec-normalized vec)
  (vec-divs vec (vec-length vec)))

(define (vec-dot vec-a vec-b)
  (let ([acc 0]
        [multiplied (vector-map (lambda (a b) (* a b)) vec-a vec-b)])
    (vector-for-each (lambda (n) (set! acc (+ acc n))) multiplied)
    acc))

(define (vec-dot-self vec)
  (vec-dot vec vec))

(define (vec3-cross a b)
  (make-vec3 (- (* (vec-y a) (vec-z b)) (* (vec-z a) (vec-y b)))
             (- (* (vec-z a) (vec-x b)) (* (vec-x a) (vec-z b)))
             (- (* (vec-x a) (vec-y b)) (* (vec-y a) (vec-x b)))))


; TODO: check correctness
(define (vec3-lerp vec-a vec-b t)
  (vector-map (lambda (a b) (lerp a b t)) vec-a vec-b ))

