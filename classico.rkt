
#lang racket
(require redex)

; Definição da gramática
(define-language G
    [nonterminal V]
    [terminal number]
    [flag *]
    [rhs (seq ...)]
    [seq (t ...) flag]
    [t nonterminal terminal]
    [production (nonterminal rhs)]
    [V variable-not-otherwise-mentioned]
    [grammar (production ...)])


(define g-->
    (reduction-relation
        G
        (--> [(nonterminal ((nonterminal t ...) seq ... ) ) production ...] 
             [(nonterminal ((t ...) seq ... ) ) production ...])
        (--> [(nonterminal ((terminal t ...) seq ... ) ) production ...] 
             [(nonterminal ( seq ...  (terminal t ...) ) ) production ...])
        (--> [(nonterminal ((nonterminal_1 t ...) seq ... ) ) production ...] 
             [(nonterminal ( seq ...  (nonterminal_1 t ...) ) ) production ...])  
        (--> [(nonterminal (flag seq ... ) ) (nonterminal_1 (( t ...) seq_1 ... ) ) production ...]
             [(nonterminal_1 (( t ...) seq_1 ... ) ) production ...  (nonterminal (flag seq ... ) )])
    )
)

; Testes

; S -> S 2 | S 4 | A
; A -> A 3 | B
; B -> B | 2
(module+ main
(traces g--> ( term (
                   (S ((1 2) (S 2) *))
                   (D ((D 3) (D 4) (D 7) (D 8) (D 9) *))
                   (B ((1 3) (1 4) *))
                   (C ((1 3) *))
                ))))

; Cvs sobre o "*" no final da sequência
; cvs sobre a criação de um novo n terminal