; Thread Utilities

(library (threads)
         (export kthread start-thread start-threads wait-thread wait-threads)
         (import (chezscheme))

(define-record-type kthread
  (fields thread
          (mutable mutex)
          (mutable condition)))

(define (start-thread proc)
  (let ([m (make-mutex)]
        [c (make-condition)])
    (make-kthread 
      (fork-thread 
        (lambda ()
          (with-mutex m
                      (proc)
                      (condition-signal c))))
      m c)))

(define (start-threads proc n)
  (if (= n 0) '()
    (cons (start-thread proc) (start-threads proc (- n 1)))))

(define (wait-thread thrd)
  (mutex-acquire (kthread-mutex thrd))
  ;(condition-wait (kthread-condition thrd) (kthread-mutex thrd))
  (mutex-release (kthread-mutex thrd)))

; This blocks until all threads have been released
(define (wait-threads lst)
  (if (not (null? lst))
    (let ([next (car lst)])
      (wait-thread next)
      (wait-threads (cdr lst))))))
