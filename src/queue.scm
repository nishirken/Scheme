(load "utils/print.scm")

(define (make-queue)
    (cons '() '()))

(define (front-ptr queue)
    (car queue))

(define (rear-ptr queue)
    (cdr queue))

(define (set-front-ptr! queue item)
    (set-car! queue item))
    
(define (set-rear-ptr! queue item)
    (set-cdr! queue item))

(define (empty-queue? queue)
    (null? (front-ptr queue)))

(define (front-queue queue)
    (if (empty-queue? queue)
        (error "FRONT вызвана с пустой очередью" queue)
        (car (front-ptr queue))))

(define (insert-queue! queue item)
    (let ((new-pair (cons item '())))
        (cond
            ((empty-queue? queue)
                (set-front-ptr! queue new-pair) (set-rear-ptr! queue new-pair) queue)
        (else (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair) queue))))

(define (delete-queue! queue)
    (cond
        ((empty-queue? queue)
            (error "DELETE! вызвана с пустой очередью" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
            queue)))

(define (print-queue queue)
    (let ((first (cdr (car queue))))
        (if (null? first)
            (display (cons (cdr queue)))
            (display (cons (caar queue) (cdr queue))))))

(define q1 (make-queue))

(print-queue (insert-queue! q1 'a))
(print q1)
(print-queue (insert-queue! q1 'b))
(print q1)
;(print-queue (delete-queue! q1))
;(print-queue (delete-queue! q1))
