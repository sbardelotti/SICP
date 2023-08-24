#lang scheme
;1.11
(define (func-n n)
  (if (< n 3)
      n
      (+ (func-n(- n 1))
         (* 2 (func-n(- n 2)))
         (* 3 (func-n(- n 3))))))


(define (func-n-iter n)
  (define (f-loop n-1 n-2 n-3 aux)
    (if (= n aux)
        n-1
        (f-loop(+ n-1 (* 2 n-2) (* 3 n-3)) n-1 n-2 (+ 1 aux))))
  (if (< n 3)
      n
      (f-loop 2 1 0 2)))

;1.12

(define (pascal row col)
  (cond ((= row 1) 1)
        ((or (= col 1) (= col row)) 1)
        (else (+ (pascal (- row 1)  (- col 1))
                 (pascal (- row 1) col)))))

(define (display-pascal-row n)
  (define (column-iter i)
    (display (pascal n i)) (display "  ")
    (if (= i n)
        (newline)
        (column-iter (+ i 1))))
  (column-iter 1))

(define (display-pascal n)
  (define (display-pascal-iter i)
    (display-pascal-row i)
    (if (= i n)
        (newline)
        (display-pascal-iter (+ i 1))))
  (display-pascal-iter 1))

;o exercicio 1.13 está além da compreensão humana