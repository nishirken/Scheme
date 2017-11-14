(load "utils/print.scm")
(load "utils/maths.scm")

(define simple-list
    (list 1 2 3 4 "last"))

; 2.17
(define (last-pair list-instance)
    (let ((list-length (length list-instance)))
        (list-ref list-instance (dec list-length))))

; (print (last-pair simple-list)

; 2.18
(define (reverse list-instance)
    (let ((list-length (length list-instance)))
        (define (iter counter result)
            (if (= counter 0)
                result
                (iter (dec counter) (append result (list (list-ref list-instance (dec counter)))))))
        (iter list-length (list))))

; (print (reverse simple-list))
