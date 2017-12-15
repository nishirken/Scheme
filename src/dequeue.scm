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
            (else (set-front-ptr! deque (cons item (front-ptr deque))) deque))))

(define (rear-insert-deque! deque item)
    (let ((new-pair (cons item '())))
        (cond
            ((empty-deque? deque)
                (set-front-ptr! deque new-pair) (set-rear-ptr! deque new-pair) deque)
        (else (set-cdr! (rear-ptr deque) new-pair)
            (set-rear-ptr! deque new-pair) deque))))

(define (front-delete-deque! deque)
    (cond
        ((empty-deque? deque)
            (error "DELETE! вызвана с пустой очередью" deque))
        (else (set-front-ptr! deque (cdr (front-ptr deque)))
            deque)))

; TODO
(define (rear-delete-deque! deque)
    (cond
        ((empty-deque? deque)
            (error "DELETE! вызвана с пустой очередью" deque))
        (else (set-front-ptr! deque (cdr (front-ptr deque)))
            deque)))

(define (print-deque deque)
    (print (car deque)))

(define deque (make-deque))
(print deque)
