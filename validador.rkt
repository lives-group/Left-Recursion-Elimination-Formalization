#lang racket

(provide (all-defined-out))

(require redex
         rackunit
         rackcheck
         "classico.rkt"
         "gerador.rkt")
         
(module+ test

  ; Gera os terminais
  (define gen:terminals
    (gen:let ([num-terminals (gen:integer-in min-terminals max-terminals)])
    (generate-terminals num-terminals)))

  ; Gera os não-terminais
  (define gen:nonterminals
    (gen:let ([num-nonterminals (gen:integer-in min-nonterminals max-nonterminals)])
    (generate-nonterminals num-nonterminals)))

  ; Gera a gramática
  (define gen:grammar
    (gen:let ([terminals gen:terminals]
              [nonterminals gen:nonterminals])
    (generate-grammar terminals nonterminals)))
  
  ; Unifica as produções geradas por um mesmo não-terminal
  (define gen:grammar-unified
    (gen:let ([grammar gen:grammar])
    (unify-productions grammar)))
  
  ; Ordena o rhs das produções
  (define gen:grammar-ordered
    (gen:let ([grammar gen:grammar-unified])
    (order-rhs grammar)))

  (check-property
    (property ([g1 gen:grammar-ordered])
      (check-true (not (empty? (apply-reduction-relation* i--> g1)))))))
