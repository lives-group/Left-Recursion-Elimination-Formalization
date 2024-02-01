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
      ; Verifica se a gramática gerada possui produções recursivas à esquerda
      (check-equal? (has-left-recursion? g1) #t)
      ; Verifica se as recursões à esquerda foram removidas
      (check-equal? (has-left-recursion? (apply-reduction-relation* i--> g1)) #f))))


;---- Funções auxiliares ----
; Verifica se uma produção é recursiva à esquerda  de forma direta
(define (is-left-recursive-d term rhs)
    (ormap
        (lambda (seq)
            (if (eq? seq '()) #f
                (if (list? seq)
                    (if (equal? (car seq) term) #t #f)
                    (if (equal? seq term) #t #f))))
        rhs))

; Verifica se uma produção possui possibilidade de recursão à esquerda indireta
(define (is-left-recursive-i order rhs)
    (ormap
        (lambda (seq)
            (if (eq? seq '()) #f
                (ormap
                    (lambda (term)
                        (define head-ord (car term))
                        (if (list? seq)
                            (if (equal? (car seq) head-ord) #t #f)
                            (if (equal? seq head-ord) #t #f)))
                    order)))
        rhs))

; Verifica se na gramática existe alguma produção recursiva à esquerda
(define (has-left-recursion? grammar)
    (if (eq? (cdr grammar) '()) #f 
        (ormap
            (lambda (prd)
            (define head (car prd))
                (define rhs (car(cdr prd)))
                (is-left-recursive-d head rhs)
                (is-left-recursive-i (car grammar) rhs))
            (car (cdr grammar)))))

