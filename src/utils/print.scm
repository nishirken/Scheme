(define (print . x)
    (begin (newline) (for-each display x) (newline)))
