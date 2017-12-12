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

; 3.21
(define (print-queue queue)
    (print (car queue)))

(define q1 (make-queue))

; (print-queue (insert-queue! q1 'a))
; (print-queue (insert-queue! q1 'b))
; (print-queue (delete-queue! q1))
; (print-queue (delete-queue! q1))

; 3.22
(define make-queue-object
    (let
        ((_queue (cons '() '())))
            (let 
                ((_front-ptr (lambda () (car _queue)))
                (_rear-ptr (lambda () (cdr _queue)))
                (_set-front-ptr! (lambda (item) (set-car! _queue item)))
                (_set-rear-ptr! (lambda (item) (set-cdr! _queue item))))
                (let
                    ((_empty-queue? (lambda () (null? (_front-ptr _queue)))))
                    (let
                        ((_insert-queue! item
                            (let ((new-pair (cons item '())))
                                (cond
                                    ((_empty-queue?)
                                        (_set-front-ptr! new-pair) (_set-rear-ptr! new-pair) _queue)
                                    (else (set-cdr! (_rear-ptr) new-pair)
                                        (_set-rear-ptr! new-pair) _queue)))))
                        (_delete-queue! (lambda ()
                            (cond
                                ((_empty-queue?) (error "DELETE! вызвана с пустой очередью" _queue))
                                (else (_set-front-ptr! (cdr (_front-ptr))) _queue)))))
                    (define (dispatch m)
                        (cond
                            ((eq? m 'front-ptr) _front-ptr)
                            ((eq? m 'rear-ptr) _rear-ptr)
                            ((eq? m 'insert-queue!) _insert-queue!)
                            ((eq? m 'delete-queue!) _delete-queue!)))
                    dispatch)))))

(define q1 make-queue-object)
(define q2 make-queue-object)
(print ((q1 'insert-queue!) 'a))
(print (q2 'insert-queue!))
