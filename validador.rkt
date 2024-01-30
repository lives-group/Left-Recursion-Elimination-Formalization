#lang racket

(require rackcheck)

; Parâmetros de entrada
(define num-terminals 8)
(define num-nonterminals 4)
(define max-rhs 3)
(define max-seq 4)


; Gera os terminais
(define gen:terminals (gen:list (gen:integer-in 5 100) #:max-length num-terminals))

; Gera uma lista de não-terminais
(define gen:non-terminals
    (gen:list
        (gen:one-of
            (list 'S 'A 'B 'C 'D 'E 'F 'G 'H 'I 'J 'K 'L 'M 'N 'O 'P 'Q 'R 'T 'U 'V 'W 'X 'Y 'Z)) #:max-length num-nonterminals))

; Sorteia um número entre 1 e 3 
; (1 = recursão direta, 2 = chance de recursão indireta, 3 = sorteio de terminal ou não-terminal)
(define gen:rule-type (gen:one-of '(1 2 3)))

(sample gen:terminals 5)

; Sorteia um terminal ou não-terminal
(define gen:rule-element (gen:one-of (list gen:terminals gen:non-terminals)))

; Sorteia um nonteminal que esteja depois do não-terminal da lista de não-terminais
(define gen:sort-non-terminal 
    (lambda (non-terminal non-terminals)
        (gen:one-of
            (filter
                (lambda (x) (not (eq? x non-terminal)))
                non-terminals))))

