
#lang racket
(require redex)

; Definição da gramática
(define-language G
    [nonterminal V]
    [terminal number ε]
    [flag *]
    [rhs (seq ...)]
    [seq (t ...) flag]
    [t nonterminal terminal]
    [production (nonterminal rhs)]
    [V variable-not-otherwise-mentioned]
    [grammar (production ...)])


(define g-->
    (reduction-relation G
          (--> [(nonterminal ((nonterminal t ...) seq ... ) ) production ...] 
               [(nonterminal ((t ... nonterminal) seq ... ) ) production ...])
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
                   (B ((B 3) (B 4) *))
                   (C ((ε) *))
                   (D ((1 3) *))
                ))))

; Cvs sobre o "*" no final da sequência
; cvs sobre a criação de um novo n terminal
; Cvs sobre ε