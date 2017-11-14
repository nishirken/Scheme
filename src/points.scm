(load "utils/maths.scm")
(load "multiple.scm")

; 2.2 points

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))

(define (make-segment start end)
    (cons start end))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))

(define (make-point x y)
    (cons x y))

(define (x-point point)
    (car point))

(define (y-point point)
    (cdr point))

(define (midpoint-segment segment)
    (let
        ((x-start (x-point (start-segment segment)))
        (y-start (y-point (start-segment segment)))
        (x-end (x-point (end-segment segment)))
        (y-end (y-point (end-segment segment))))
        (make-point (average x-start x-end) (average y-start y-end))))

(define segment
    (make-segment (make-point 1 2) (make-point 5 6)))

; (print-point (midpoint-segment segment))

; 2.3 rectangle
(define (make-rectangle a b c)
    (let
        ((left-right (make-segment a b))
        (top-bottom (make-segment b c)))
        (cons left-right top-bottom)))

(define (left-right-side rectangle)
    (car rectangle))

(define (top-bottom-side rectangle)
    (cdr rectangle))

(define (segment-length segment)
    (let
        ((x-start (x-point (start-segment segment)))
        (y-start (y-point (start-segment segment)))
        (x-end (x-point (end-segment segment)))
        (y-end (y-point (end-segment segment))))
        (let
            ((ax (- x-end x-start))
            (ay (- y-end y-start)))
            (sqrt (+ (square ax) (square ay))))))

(define (perimeter rectangle)
    (+
        (double (segment-length (left-right-side rectangle)))
        (double (segment-length (top-bottom-side rectangle)))))

(define (area rectangle)
    (*
        (segment-length (left-right-side rectangle))
        (segment-length (top-bottom-side rectangle))))

(define rectangle
    (make-rectangle (make-point 3 4) (make-point 5 4) (make-point 5 1)))

; (display (perimeter rectangle))
; (display (area rectangle))
