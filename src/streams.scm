(load "utils/print.scm")
(load "divisions.scm")
(load "utils/maths.scm")
(load "sqrtIter.scm")

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

; 3.57
(define (expand num den radix)
    (cons-stream
        (quotient (* num radix) den)
        (expand (remainder (* num radix) den) den radix)))

(expand 1 7 10) ; (1 . (promise (expand 3 7 10)))
(expand 3 8 10) ; (3 . (promise (expand 6 7 10)))

; 3.59
(define (integrate-series stream)
    (stream-map / stream integers))

(define exp-series
    (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
    (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
    (cons-stream 0 (integrate-series cosine-series)))

; 3.60
(define (mul-series s1 s2)
    (mul-streams s1 s2))

(define (square-series s)
    (mul-series s s))

; (print (add-streams (square-series cosine-series) (square-series sine-series)))

; 3.61
(define (invert-unit-series s)
    (cons-stream 1
        (stream-map - ones
            (mul-series (stream-cdr s) (invert-unit-series s)))))

; (print (mul-streams (invert-unit-series sine-series) sine-series))

; 3.62
(define (div-series s1 s2)
    (let ((c (stream-car s2)))
        (if (zero? c)
            (error "Can't divide on zero")
            (scale-stream
                (mul-series s1 (invert-unit-series (scale-stream s2 (/ 1 c)))) (/ 1 c)))))

; (print (div-series sine-series cosine-series))

(define (euler-transform s)
    (let
        ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
        (cons-stream
            (- s2 (/ (square (- s2 s1)) (+ s0 (* -2 s1) s2)))
            (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
    (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
    (stream-map stream-car (make-tableau transform s)))

; 3.63
(define (sqrt-stream x)
    (define guesses
        (cons-stream 1.0
            (stream-map (lambda (guess) (improve guess x)) guesses)))
    guesses)

; 3.64
(define (good-enough? prev-guess guess tolerance)
    (< (abs (- prev-guess guess)) tolerance))

(define (stream-limit s tolerance)
    (let
        ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1)))
        (if (good-enough? s0 s1 tolerance)
            s1
            (stream-limit (stream-cdr s) tolerance))))

(define (sqrt-s x tolerance)
    (stream-limit (sqrt-stream x) tolerance))

; (print (sqrt-s 9 0.1))
; (print (sqrt-s 81 0.1))
; (print (sqrt-s 4 0.1))

; 3.65
(define (logo-summands n)
    (cons-stream (/ 1.0 n)
        (stream-map - (logo-summands (inc n)))))

(define logo-stream
    (partial-sums (logo-summands 1)))

(define accelerated-logo-stream
    (accelerated-sequence euler-transform logo-stream))

; (print (stream-limit accelerated-logo-stream 0.0001))

; 3.67
(define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
            (interleave s2 (stream-cdr s1)))))

; TODO
(define (pairs-no-condition s t)
    (cons-stream
        (list (stream-car s) (stream-car t))
        (interleave
            (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
            (pairs (stream-cdr s) (stream-cdr t)))))
          