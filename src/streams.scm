(load "utils/print.scm")
(load "divisions.scm")

(define (display-line x)
    (begin (display x) (newline)))

(define (print-stream s)
    (stream-for-each display-line s))

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

; Infinite streams
(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
    (cons-stream 0
        (cons-stream 1
            (add-streams (stream-cdr fibs) fibs))))

(define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream))

; 3.53
(define s (cons-stream 1 (add-streams s s)))
; (1 2 4 8 16)...
; (print (stream-ref s 3)) ; 8

; 3.54
(define (mul-streams s1 s2)
    (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams factorials integers)))
; (print (stream-ref factorials 6))
    
; 3.55
(define (partial-sums s)
    (add-streams s (cons-stream 0 (partial-sums s))))

(define x (partial-sums integers))
; (print (stream-ref x 5))

; 3.56
(define (merge s1 s2)
    (cond
        ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
            (let ((s1car (stream-car s1)) (s2car (stream-car s2)))
                (cond
                    ((< s1car s2car)
                        (cons-stream s1car (merge (stream-cdr s1) s2)))
                    ((> s1car s2car)
                        (cons-stream s2car (merge s1 (stream-cdr s2))))
                    (else
                        (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define S (cons-stream 1
    (merge (scale-stream S 2) (merge (scale-stream S 2) (scale-stream S 2)))))

; (print (stream-ref S 16))
