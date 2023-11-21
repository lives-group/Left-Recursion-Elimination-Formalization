#lang racket

; Gerador de gramáticas recursivas à esquerda, como o exemplo abaixo:
;(
;  (S ((B 2) (A 4) (2)))
;  (C ((A) (S 2)))
;  (B ((S 2) (B 3)))
;  (A ((C A) (S 2)))
; )

; Parâmetros de entrada
(define num-terminals 8)
(define num-nonterminals 4)
(define max-rhs 3)
(define max-seq 4)

; Gera os terminais
(define (generate-terminals)
  (let loop ((n 1) (acc '()))
    (if (> n num-terminals)
        acc
        (loop (+ n 1) (cons n acc)))))

; Gera os não-terminais
(define (generate-nonterminals)
    (let loop ((n 0) (acc '()))
        (if (> n num-nonterminals)
            acc
            (loop (+ n 1) (cons (integer->char (+ (modulo (+ n 1) 26) (if (= n 0) 82 63))) acc)))))

; Sorteia o tamanho do rhs
(define (get-rhs-size)
  (+ 1 (random max-rhs)))

; Sorteia o tamanho da sequência
(define (get-seq-size)
  (+ 1 (random max-seq)))

; Sorteia um terminal ou não-terminal
(define (get-term terminals nonterminals)
    (if (< (random 2) 1)
        (list-ref terminals (random num-terminals))
        (list-ref nonterminals (random num-nonterminals))))

; Sorteia um número entre 1 e 3 (1 = recursão direta, 2 = chance de recursão indireta, 3 = sorteio de terminal ou não-terminal)
(define (get-recursion-type)
    (+ 1 (random 3)))

(display (get-term (generate-terminals) (generate-nonterminals)))