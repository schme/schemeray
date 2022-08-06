; TODO: Change place of list procedures

; LIST UTILITIES

(define (list-fill! lst object)
  (if (not (null? lst))
    (begin
      (set-car! lst object)
      (list-fill! (cdr lst) object))))

(define (list-set! lst element object)
  (if (= element 0)
    (set-car! lst object)
    (list-set! (cdr lst) (- element 1) object)))

(define (list-get lst element)
  (if (= element 0)
    (car lst)
    (list-get (cdr lst) (- element 1))))

; IMAGE

(define (make-image width height)
  (if (= height 0)
    (list)
    (cons (make-list width (list)) (make-image width (- height 1)))))

(define (image-fill! image color)
  (if (not (null? image))
    (begin
      (list-fill! (car image) color) (image-fill! (cdr image) color))))

(define (image-set! image x y color)
  (list-set! (list-get image y) x color))

(define (image-get image x y)
  (list-get (list-get image y) x))

(define (image-width image)
  (if (not (null? image))
    (length (car image))
  0))

(define (image-height image)
  (if (not (null? image))
    (length image)
    0))

(define (image-fill-rect! image x y w h color)
  (image-set! image x y color)
  (if (> w 0)
    (image-fill-rect! image (+ x 1) y (- w 1) h color))
  (if (> h 0)
    (image-fill-rect! image x (+ y 1) w (- h 1) color)))


; PPM
(define (write-ppm image file)
  (let ([height (image-height image)]
        [width (image-width image)])
    (define (write-image image port)
      (define (write-row row port)
        (define (write-colour colour port)
          (if (not (null? colour))
            (begin
              (put-u8 port (car colour))
              (write-colour (cdr colour) port))))
        (if (not (null? row))
          (begin
            (write-colour (car row) port)
            (write-row (cdr row) port))))
      (if (not (null? image))
        (begin
          (write-row (car image) port)
          (write-image (cdr image) port))))
    (call-with-port (open-file-output-port file (file-options no-fail) (buffer-mode block) #f)
                    (lambda (port)
                      (let ([header-string (string-append "P6 " (number->string width) " " (number->string height) " " (number->string 255) "\n")])
                        (put-bytevector port (string->bytevector header-string (make-transcoder (latin-1-codec))))
                        (write-image image port)
                        (close-port port))))))

