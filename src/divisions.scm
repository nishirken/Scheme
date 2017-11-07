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
            ((= counter 3) (display (- (runtime-ms) start-time)))
            ((prime? current-number)
                (begin
                    (display "Prime number: ")
                    (display current-number)
                    (newline)
                    (search-iter (+ current-number 1) (+ counter 1) start-time)))
            (else (search-iter (+ current-number 1) counter start-time))))
    (search-iter limit 0 (runtime-ms)))
