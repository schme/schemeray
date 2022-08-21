; LIST UTILITIES

(define (list-fill! lst object)
  (if (not (null? lst))
    (begin
      (set-car! lst object)
      (list-fill! (cdr lst) object))))

(define (list-set! lst element object)
  (if (not (null? lst))
    (if (= element 0)
      (set-car! lst object)
      (list-set! (cdr lst) (- element 1) object))))

(define (list-get lst element)
  (if (null? lst)
    '()
    (if (= element 0)
      (car lst)
      (list-get (cdr lst) (- element 1)))))

