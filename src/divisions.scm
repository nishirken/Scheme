(load "utils/print.scm")
(load "utils/runtime.scm")

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (smallest-divisor n)
    (find-divisor n 2))

(define (next x)
    (if (= x 2)
        3
        (+ x 2)))

(define (find-divisor n test-divisor)
    (cond
        ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
    (= (remainder b a) 0))

(define (prime? n)
    (= n (smallest-divisor n)))

;fast prime
(define (expmod base exp m)
    (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;report time
(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
    
(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

(define (search-next-primes limit)
    (define (search-iter current-number counter start-time)
        (cond
            ((= counter 3) (display (- (runtime) start-time)))
            ((fast-prime? current-number 10)
                (begin
                    (display "Prime number: ")
                    (display current-number)
                    (newline)
                    (search-iter (+ current-number 1) (+ counter 1) start-time)))
            (else (search-iter (+ current-number 1) counter start-time))))
    (search-iter limit 0 (runtime)))
