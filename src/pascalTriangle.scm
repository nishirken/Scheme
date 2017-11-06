(load "utils/newLine.scm")
(load "utils/maths.scm")

(define (pascalTriangle line-umber number-in-line)
    (/ (factorial line-umber) (* (factorial number-in-line) (factorial (- line-umber number-in-line)))))

(newLine)
(write (pascalTriangle 4 2))
(newLine)
