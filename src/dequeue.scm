(load "utils/print.scm")

; 3.23
(define (make-deque)
    (cons '() '()))

(define (front-ptr deque)
    (car deque))

(define (rear-ptr deque)
    (cdr deque))

(define (set-front-ptr! deque item)
    (set-car! deque item))

(define (set-rear-ptr! deque item)
    (set-cdr! deque item))

(define (empty-deque? deque)
    (null? (front-ptr deque)))

(define (front-insert-deque! deque item)
    (let ((new-pair (cons item '())))
        (cond
            ((empty-deque? deque)
                (set-front-ptr! deque new-pair) (set-rear-ptr! deque new-pair) deque)
        (else (set-cdr! (rear-ptr deque) new-pair)
            (set-rear-ptr! deque new-pair) deque))))

(define (rear-insert-deque!)
    ())

(define (front-delete-deque!)
    ())

(define (rear-delete-deque!)
    ())

(define deque (make-deque))
(print (empty-deque? deque))
