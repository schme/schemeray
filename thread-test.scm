(load "utilities.scm")
(load "image.scm")
(load "thread.scm")

(define imagebuffer (make-image 8 5))
(define current-pixel 0)

(define (fib n)
  (cond  ((= n 0) 0)
         ((= n 1) 1)
         (else
           (+ (fib (- n 1)) (fib (- n 2))))))

(define (thread-main)
  (let* ([width (image-width imagebuffer)]
        [height (image-height imagebuffer)]
        [pixels (* width height)]
        [x (remainder current-pixel width)] 
        [y (floor (/ current-pixel width))])
    (if (< current-pixel pixels)
      (begin
        (display (format "x: ~s y: ~s current-pixel: ~s\n" x y current-pixel))
        (set! current-pixel (+ current-pixel 1))
        (image-set! imagebuffer x y (fib current-pixel))
        (thread-main)))))


; Usage

(thread-main)

(time
  (let ([threads (start-threads thread-main 8)])
    (wait-threads threads)))
