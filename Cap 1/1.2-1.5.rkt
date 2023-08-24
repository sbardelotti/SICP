#lang scheme
;1.2
(/
 (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
 (* 3
    (- 6 2)
    (- 2 7)))

;1.3
(define (square x)
  (* x x))

(define (sum-square x y)
  (+ (square x) (square y)))

(define (sum-large x y z)
  (cond ((and (<= x y) (<= x z)) (sum-square y z))
        ((and (<= y x) (<= y z)) (sum-square x z))
        (else (sum-square x y))))
;1.5
#|
A chave é perceber que (define (p) (p)) define uma função que avalia a si mesma.
Caso o interpretador tente avaliar isso ocorrerá um laço infinito que é o que acontece na Ordem de aplicativo.
que tem um funcionamento de “avaliar os argumentos e depois aplicar”.

Com um interpretador que usa avaliação de ordem normal, o interpretador irá “expandir totalmente e depois reduzir”.
Nesse modelo, o interpretador não avaliará os operandos até que seus valores sejam realmente necessários.
Nesse caso (teste 0(p)) como o primeiro parâmetrp é igual a 0, o interpretador devolverá 0 para gente
e o (p) nunca será avaliado
|#