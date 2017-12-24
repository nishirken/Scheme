(load "utils/print.scm")

(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream
            low
            (stream-enumerate-interval (+ low 1) high))))

; 3.50
(define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (cons-stream
            (apply proc (map stream-car argstreams))
            (apply stream-map
                (cons proc (map stream-cdr argstreams))))))

; 3.51
(define (show x)
    ; (print x)
    x)

(define x (stream-map show (stream-enumerate-interval 0 10))) ; 0

(stream-ref x 5) ; 5
(stream-ref x 7) ; 7

; 3.52
(define sum 0)

(define (accum x)
    (set! sum (+ x sum))
    sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20))) ; 1
; (print sum)

(define y (stream-filter even? seq))
; (print sum)

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
    seq))
; (print sum)

(stream-ref y 3)
; (print sum)
