#lang racket

(provide (all-defined-out))

(require redex
         rackunit
         rackcheck
         "classico.rkt"
         "gerador.rkt"
         "../cfg-entry-generator/main.rkt")
         
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

 (check-property (make-config #:tests 10
                              #:deadline (* (+ (current-inexact-milliseconds) 3600000) 24))
    (property recursaoRemovida ([g1 gen:grammar-ordered])
      ; Aplica a remoção de recursão à esquerda
      (define g2 (car (apply-reduction-relation* i--> g1)))        
      ; Verifica se as recursões à esquerda foram removidas
      (check-equal? (has-left-recursion? g2) #f)
      ; Verifica se termos aceitos por g1 são aceitos por g2
      (check-equal? (accepts? g1 g2) #t)
      ; Verifica se termos aceitos por g2 são aceitos por g1
      (check-equal? (accepts? g2 g1) #t)
      ; Verifica se os termos NÃO aceitos por g1 são aceitos por g2
      (check-equal? (not-accepts? g1 g2) #t)
      ; Verifica se os termos NÃO aceitos por g2 são aceitos por g1
      (check-equal? (not-accepts? g2 g1) #t)   
      )))

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

; Retorna os elementos de uma lista anteriores a um elemento
(define (before-list list element)
  (cond
    [(null? list) '()] 
    [(equal? (car (car list)) element) '()] 
    [else (cons (car list) (before-list (cdr list) element))]))

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
                  (before-list order (car seq)))
            ))
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

; Verifica se a gramática se termos aceitos por gf são aceitos por gs
(define (accepts? gf gs)
    (define g-first (car (cdr gf)))
    (define g-second (car (cdr gs)))

    ; Gera os termos de gf
    (define terms (generate-terms g-first))
    (ormap
      (lambda (term)
        ; Verifica se o termo é aceito por gs
        (if (accepts-term? term g-second) #t #f))
      terms))

; Gera os termos de uma gramática
(define (generate-terms grammar)
    ; para fim de teste, retorna os numeros de 0 a 100 que sejam par
    (range 0 100 2))

; Verifica se a gramática gs não aceita termos que não são aceitos por gf   
(define (not-accepts? gf gs)
    (define g-first (car (cdr gf)))
    (define g-second (car (cdr gs)))

    ; Gera os termos que não são aceitos por gf
    (define not-terms (generate-not-terms g-first))

    (ormap
        (lambda (term)
            ; Verifica se o termo é aceito por gs
            (if (accepts-term? term g-second) #f #t))
        not-terms))
  
; Gera os termos que não são aceitos por uma gramática
(define (generate-not-terms grammar)
    ; para fim de teste, retorna os numeros de 0 a 100 que sejam ímpar
    (range 1 100 2))

; Verifica se um termo é aceito por uma gramática
(define (accepts-term? term grammar)
    ; para fim de teste, se for um número par, retorna true, senão, false
    (if (even? term) #t #f))
