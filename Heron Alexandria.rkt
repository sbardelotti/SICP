#lang scheme
(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (avarage a b)
  (/ (+ a b)
     2))

(define tolerance 0.001)
(define (close-enough? old new)
    (< (abs (- old new)) tolerance))
 

(define (sqrt x)
  (define tolerance 0.00001)
  (define (good-enough? y)
    (< (abs (- (* y y) x)) tolerance))
  (define (improve y)
    (avarage (/ x y) y))
  (define (try y)
    (if (good-enough? y)
        y
        (try (improve y))))
  (try 1))

#|Com ponto fixo|#

(define (sqrt-point x)
  (fixed-point
   (lambda (y) (avarage (/ x y) y))
   1))

;o ponto fixo da função computada pelo procedimento "f" 
(define (fixed-point f start)
  (define (iter old new)
    (if (close-enough? old new)
        new
        (iter new (f new))))
  (iter start (f start)))

;

(define avarage-damp
  (lambda (f);parâmetro
    (lambda (x) (avarage (f x) x))));retorno

(define (sqrt-damp x)
  (fixed-point
    (avarage-damp(lambda (y) (/ x y)))
    1))

;

(define (square x)
  (* x x))

(define dx 0.0000001)

(define derivative
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx))
            (f x))
         dx))))

(define (newton f guess)
  (define derivative-f (derivative f))
  (fixed-point
    (lambda (x) (- x (/ (f x)(derivative-f x))))
     guess))

(define (sqrt-newton x)
  (newton (lambda (y) (- x (square y)))
          1))




