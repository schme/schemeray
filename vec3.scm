; vec3

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

(define (vec-divs vec-a val)
  (map (lambda (a) (/ a val)) vec-a))

(define (vec-muls vec-a val)
  (map (lambda (a) (* a val)) vec-a))

(define (vec-add vec-a vec-b)
  (map (lambda (a b) (+ a b)) vec-a vec-b))

(define (vec-sub vec-a vec-b)
  (map (lambda (a b) (- a b)) vec-a vec-b))

(define (vec-mul vec-a vec-b)
  (map (lambda (a b) (* a b)) vec-a vec-b))

(define (vec-div vec-a vec-b)
  (map (lambda (a b) (/ a b)) vec-a vec-b))


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
