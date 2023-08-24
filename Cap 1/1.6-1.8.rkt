;1.6
#lang scheme
(define (new-if predicate then-clause else-clause)
 (cond (predicate then-clause)
 (else else-clause)))

#|
Em uma função cada subexpressão de parâmetro será avaliada antes que o procedimento seja aplicado.
Como a função sqrt-iter se auto-invoca (sqrt-iter (improve guess x) x) isso cria um loop infinito
|#

;1.7
#|
Para números muito pequenos, a raiz quadrada pode ser um valor próximo de zero.
Se aplicarmos o teste "good-enough" usando uma precisão limitada, a diferença entre os valores estimados de uma iteração para outra pode ser maior do que a própria estimativa da raiz quadrada.
Por exemplo, considere a raiz quadrada de 0,0001.
Se usarmos um palpite inicial de 0,1 e aplicarmos o teste "good-enough" com uma precisão de 0,01, a diferença entre iterações sucessivas pode ser maior do que 0,01, resultando em uma estimativa imprecisa.

Da mesma forma, para números muito grandes, a raiz quadrada pode ser um valor extremamente grande.
Nesse caso, o teste "good-enough" também pode falhar porque a diferença entre os valores estimados de uma iteração para outra pode ser maior do que a própria estimativa da raiz quadrada.

Uma estratégia alternativa mais eficaz é acompanhar como o palpite da raiz quadrada muda de uma iteração para outra e parar quando a mudança for uma fração muito pequena do próprio palpite.
Isso garante uma convergência mais precisa para a raiz quadrada, independentemente do tamanho do número.
Por exemplo, se o palpite atual for x, podemos calcular o próximo palpite como (x + n / x) / 2, onde n é o número para o qual estamos calculando a raiz quadrada.

Um modo de resolvir isso está a seguir:
|#

(define (square x) (* x x))

(define (good-enough? previous-guess guess)
  (< (abs (/ (- guess previous-guess) guess)) 0.00000000001))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; Modo antigo:
#|
(define (good-enough? guess x)
(< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
  guess
  (sqrt-iter (improve guess x) x)))
|#

;good-enough não depende de x agora, ainda é impreciso só que muito menos do que da outra forma

;1.8

(define (cbrt-iter guess x)
  (if (good-enough? guess (improve-cbrt guess x))
      guess
      (cbrt-iter (improve-cbrt guess x) x)))

(define (improve-cbrt guess x)
  (/
   (+
    (/ x (* guess guess))
    (* 2 guess))
   3))


(define (cbrt x)
  (cbrt-iter 1.0 x))