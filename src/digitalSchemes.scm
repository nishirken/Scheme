(load "utils/print.scm")

(define (make-wire)
    (let ((signal-value 0) (action-procedures '()))
        (define (set-my-signal! new-value)
            (if (not (= signal-value new-value))
                (begin (set! signal-value new-value)
                    (call-each action-procedures))
                'done))
        (define (accept-action-procedure! proc)
            (set! action-procedures (cons proc action-procedures))
            (proc))
        (define (dispatch m)
            (cond
                ((eq? m 'get-signal) signal-value)
                ((eq? m 'set-signal!) set-my-signal!)
                ((eq? m 'add-action!) accept-action-procedure!)
                (else (error "Неизвестная операция -- WIRE" m))))
        dispatch))

(define (call-each procedures)
    (if (null? procedures)
        'done
        (begin ((car procedures)) (call-each (cdr procedures)))))

(define (get-signal wire)
    (wire 'get-signal))

(define (set-signal! wire new-value)
    ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
    ((wire 'add-action!) action-procedure))

(define (half-adder a b s c)
    (let ((d (make-wire)) (e (make-wire)))
        (or-gate a b d)
        (and-gate a b c)
        (inverter c e)
        (and-gate d e s)
        'ok))

(define (full-adder a b c-in sum c-out)
    (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
            (half-adder b c-in s c1)
            (half-adder a s sum c2)
            (or-gate c1 c2 c-out)
            'ok))

(define (inverter input output)
    (define (invert-input)
        (let
            ((new-value (logical-not (get-signal input))))
            (after-delay inverter-delay
                (lambda ()
                    (set-signal! output new-value)))))
    (add-action! input invert-input)
    'ok)

(define (logical-not s)
    (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Неправильный сигнал" s))))

(define (and-gate a1 a2 output)
    (define (and-action-procedure)
        (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
            (after-delay and-gate-delay
                    (lambda ()
                        (set-signal! output new-value)))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure) 'ok)

; 3.28
(define (logical-and x y)
    (cond
        (or (and (= x 0) (= y 0))
            (and (= x 0) (= y 1))
            (and (= x 1) (= y 0)) 0)
        ((and (= x 1) (= y 1)) 1)
        (else (error "Неправильный сигнал") '(x y))))

(define (logical-or x y)
    (cond
        (or (and (= x 1) (= y 1))
            (and (= x 0) (= y 1))
            (and (= x 1) (= y 0)) 0)
        ((and (= x 0) (= y 0)) 0)
        (else (error "Неправильный сигнал") '(x y))))

(define (or-gate a1 a2 output)
    (define (or-action-procedure)
        (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
            (after-delay or-gate-delay
                    (lambda ()
                        (set-signal! output new-value)))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure) 'ok)

; 3.29
(define (or-gate-v2 a1 a2 output) 
    (let ((c (make-wire)) 
          (d (make-wire))
          (e (make-wire)) 
          (f (make-wire)
          (g (make-wire)))) 
        (and-gate a1 a1 d) 
        (and-gate a2 a2 e) 
        (inverter d f) 
        (inverter e g) 
        (and-gate f g c) 
        (inverter c output) 
        'ok))

; 3.30
(define (ripple-carry-adder A B S c)
    (if (null? S)
        'ok
        ((full-adder (car A) (car B) c (car S) c)
    (ripple-carry-adder (cdr A) (cdr B) (cdr S) c))))

(define (after-delay delay action)
    (add-to-agenda!
        (+ delay (current-time the-agenda))
        action
        the-agenda))
