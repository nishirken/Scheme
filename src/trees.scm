(load "utils/print.scm")
(load "utils/maths.scm")
(load "compose.scm")
(load "divisions.scm")

(define (atom? x)
    (and (not (null? x))
        (not (pair? x))))

(define (count-leaves x)
    (cond
        ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
        (count-leaves (cdr x))))))

; 2.24
; (list 1 (list 2 (list 3 4)))
;        /\
;       1  (list 2 (list 3 4))
;                 /\
;                2  (list 3 4)
;                        /\              
;                       3  4
; 
; (print (count-leaves (list 1 (list 2 (list 3 4))))) 4

; 2.25
; (define list-test (list 1 3 (list 5 7) 9)) (print (car (cdr (car (cdr (cdr list-test))))))
; (define list-test (list (list 7))) (print (car (car list-test)))
; (define list-test (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
; (print (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list-test)))))))))))))

; 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

; (print (append x y))
; (1 2 3 4 5 6)
; (print (cons x y))
; ((1 2 3) 4 5 6)
; (print (list x y))
; ((1 2 3) (4 5 6))

; 2.27
(define (deep-reverse seq)
    (if (list? seq)
        (map deep-reverse (reverse seq))
        seq))

; (print (deep-reverse (list 2 (list 3 1) (list 7 (list 2 5 6) 5 1))))

; 2.28
(define (fringe seq)
    (cond
        ((null? seq) '())
        ((atom? seq) (list seq))
        (else (append (fringe (car seq)) (fringe (cdr seq))))))

; (print (fringe (list (list (list 1 2) 3) (list 2 3) 7 (list 1) 3 13)))

; 2.31
(define (tree-map fn tree)
    (map (lambda (sub-tree)
        (if (list? sub-tree)
            (tree-map fn sub-tree)
            (fn sub-tree))) tree))

; (print (tree-map square (list 1 2 (list 5 1) (list 3 7 (list 9 2)))))

; 2.32
(define (subsets set)
    (if (null? set)
        (list '())
        (let ((rest (subsets (cdr set))))
            (append rest (map (lambda (x) (cons (car set) x)) rest)))))

; (print (subsets (list 1 2 3 4)))

; 2.33
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
            (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (map-acc p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

; (print (map-acc square (list 3 7 4)))


(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

; (print (append (list 3 1) (list 4 5)))

(define (length sequence)
    (accumulate (lambda (x y) (inc y)) 0 sequence))

; (print (length (list 3 2 9 1 2)))

; 2.34
(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coefficient accumulator)
        (+ this-coefficient (* x accumulator)))
        0
        coefficient-sequence))

; (print (horner-eval 2 (list 1 3 0 5 0 1)))

; 2.35
(define (count-leaves-map t)
    (accumulate + 0
        (map (lambda (x)
            (if (list? x)
                (count-leaves-map x)
                1)) t)))

; (print (count-leaves-map (list 1 3 5 2 (list 5 2) (list 3 1) 2 3)))

; 2.36
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op init (map (lambda (sub-seq) (car sub-seq)) seqs))
            (accumulate-n op init (map (lambda (sub-seq) (cdr sub-seq)) seqs)))))

; (print (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))))

; 2.37
; | 1 2 3 4 |
; | 4 5 6 6 |
; | 6 7 8 9 |
(define matrix
    (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

; (print (dot-product (list 1 2 3) (list 1 2 3)))

(define (matrix-*-vector matrix vector)
    (map (lambda (row) (dot-product row vector)) matrix))

; (print (matrix-*-vector (list (list 1 2) (list 3 5)) (list 1 3)))

; | 1 4 6 |
; | 2 5 7 |
; | 3 6 8 |
; | 4 6 9 |
(define (transpose matrix)
    (map reverse (accumulate-n (lambda (row acc) (append acc (list row))) (list) matrix)))

(define (transpose-map matrix)
    (apply map list matrix))

; (print (transpose matrix))
; (print (transpose-map (list (list 1 2 3 4) (list 4 5 6 6))))


(define (matrix-*-matrix m n)
    (let ((cols (transpose-map n)))
        (map (lambda (row) (matrix-*-vector n row)) m)))

; (print (matrix-*-matrix (list (list 1 2 3) (list 4 5 6)) (list (list 5 6) (list 3 6) (list 5 1))))

; 2.38
(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest)) (cdr rest))))
    (iter initial sequence))

; (print (fold-right / 1 (list 1 2 3)))
; (print (fold-left / 1 (list 1 2 3)))
; (print (fold-right list '() (list 1 2 3)))
; (print (fold-left list '() (list 1 2 3)))


; 2.39
(define (reverse-right sequence)
    (fold-right (lambda (x y) (append y (list x))) '() sequence))

; (print (reverse-right (list 1 2 3 4)))

(define (last sequence)
    (if (null? (cdr sequence))
        (car sequence)
        (last (cdr sequence))))

(define (reverse-left sequence)
    (fold-left (lambda (x y) (append x (list y))) '() (reverse-right sequence)))

; (print (reverse-left (list 1 2 3 4)))

(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
    (if (> low high)
        '()
        (cons low (enumerate-interval (inc low) high))))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; 2.40
(define (unique-pairs n)
    (flatmap (lambda (i)
        (map (lambda (j)
            (list i j)) (enumerate-interval 1 (dec i))))
    (enumerate-interval 1 n)))

; (print (unique-pairs 7))
(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum? (unique-pairs n))))

; (print (prime-sum-pairs 3))

; 2.41
(define (pair-sum pair)
    (+ (car pair) (cadr pair)))

(define (same-sum? pair s)
    (= (pair-sum pair) s))

(define (same-sum-pairs n s)
    (map make-pair-sum
        (filter (lambda (x) (same-sum? x s)) (unique-pairs n))))

; (print (same-sum-pairs 9 6))
