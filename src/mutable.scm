(load "utils/print.scm")
(load "utils/maths.scm")

(define (make-account balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            "Недостаточно денег на счете"))
    (define (deposit amount)
        (set! balance (+ balance amount)) balance)
    (define (dispatch m)
        (cond
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Неизвестный вызов -- MAKE-ACCOUNT" m))))
    dispatch)

; 3.1
(define (make-accumulator initial)
    (lambda (amount)
        (begin (set! initial (+ amount initial)) initial)))

(define A (make-accumulator 10))
(define B (make-accumulator 5))
; (print (A 5))
; (print (A 15))
; (print (B 9))
; (print (B 2))

; 3.2
(define (make-monitored f)
    (let ((counter 0))
        (define (mf . args)
            (cond
                ((eq? (car args) 'how-many-calls?) counter)
                ((eq? (car args) 'reset-count) (begin (set! counter 0) counter))
                (else (begin (set! counter (inc counter)) (apply f args)))))
    mf))

(define s
    (make-monitored +))
; (print (s 'how-many-calls?))
; (print (s 1 2 3 1))
; (print (s 1 1))
; (print (s 'how-many-calls?))
; (print (s 'reset-count))
; (print (s 'how-many-calls?))

; 3.3
(define (make-protected-account balance initial-password)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            "Недостаточно денег на счете"))
    (define (deposit amount)
        (set! balance (+ balance amount)) balance)
    (define (dispatch m password)
        (if (eq? password initial-password)
            (cond
                ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Неизвестный вызов -- MAKE-ACCOUNT" m)))
            (error "Password failed" password)))
    dispatch)

(define acc
    (make-protected-account 100 'qwerty))

; (print ((acc 'withdraw 'qwerty) 20))
; (print ((acc 'deposit 'qwerty) 30))
; (print ((acc 'withdraw 'qq) 30))

; 3.4
(define (make-protected-account-alert balance initial-password)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount)) balance)
            "Недостаточно денег на счете"))
    (define (deposit amount)
        (set! balance (+ balance amount)) balance)
    (define (change-password new-password)
        (begin (set! initial-password new-password) new-password))
    (let ((invalid-password-counter 0))
        (lambda (m password)
            (cond
                ((eq? password initial-password)
                    (begin (set! invalid-password-counter 0)
                        (cond
                            ((eq? m 'new-password) (lambda (new-password) (change-password new-password)))
                            ((eq? m 'withdraw) withdraw)
                            ((eq? m 'deposit) deposit)
                            (else (error "Неизвестный вызов -- MAKE-ACCOUNT" m)))))
                ((= invalid-password-counter 4) (error "Password failed, We've called the police!!!"))
                (else (begin
                    (set! invalid-password-counter (inc invalid-password-counter))
                    (lambda (args) (list "Password failed" "Remained attemts: " (- 5 invalid-password-counter)))))))))

(define acc-alert
    (make-protected-account-alert 200 'qq))
; (print ((acc-alert 'withdraw 'qq) 20))
; (print ((acc-alert 'withdraw '1) 20))
; (print ((acc-alert 'withdraw '11) 20))
; (print ((acc-alert 'withdraw '1q) 20))
; (print ((acc-alert 'withdraw '1ee) 20))
; (print ((acc-alert 'withdraw '1a) 20))

; 3.7
(define my-acc
    (make-protected-account-alert 200 'pass))

(define (make-joint account old-password new-password)
    (begin ((account 'new-password old-password) new-password)) account)

(define peter-acc
    (make-joint my-acc 'pass 'new-pass))

; (print ((peter-acc 'withdraw 'new-pass) 20))
; (print ((my-acc 'deposit 'new-pass) 20))
; (print ((peter-acc 'withdraw 'new-pass) 30))
; (print ((my-acc 'deposit 'new-pass) 30))

; 3.8
(define (make-f)
    (let ((current 0))
        (lambda (number)
            (let ((var current))
                (set! current number) var))))

(define f (make-f))

; (print (+ (f 1) (f 0)))

(define (mystery x)
    (define (loop x y)
        (if (null? x)
            y
            (let ((temp (cdr x)))
                (set-cdr! x y)
                (loop temp x))))
    (loop x '()))

; (print (mystery '(a b c)))

