#lang racket

(provide (all-defined-out))

(require redex
         rackunit
         rackcheck
         "classico.rkt"
         "gerador.rkt"
         "struct.rkt"
         "../cfg-entry-generator/main.rkt")

; Quantidade de testes
(define num-tests 100)
; Quatidade de palavras 
(define num-words 100)

(module+ test
  ; Gera os terminais
  (define gen:trms
    (gen:let ([num-trms (gen:integer-in min-trms max-trms)])
    (generate-trms num-trms)))

  ; Gera os não-terminais
  (define gen:nts
    (gen:let ([num-nts (gen:integer-in min-nts max-nts)])
    (generate-nts num-nts)))

  ; Gera a gramática
  (define gen:grammar
    (gen:let ([trms gen:trms]
              [nts gen:nts])
    (generate-grammar trms nts)))
  
  ; Unifica as produções geradas por um mesmo não-trm
  (define gen:grammar-unified
    (gen:let ([grammar gen:grammar])
    (unify-prds grammar)))
  
  ; Ordena o rhs das produções
  (define gen:grammar-orded
    (gen:let ([grammar gen:grammar-unified])
    (ord-rhs grammar)))

 (check-property (make-config #:tests num-tests
                              #:deadline (* (+ (current-inexact-milliseconds) 3600000) 24))
    (property recursaoRemovida ([g1 gen:grammar-orded])
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
; Verifica se na gramática existe alguma produção recursiva à esquerda
(define (has-left-recursion? grammar)
  (define prds (car (cdr grammar)))
  (define ords (car grammar))
   
  (ormap
    (lambda (prd)
      (define head (car prd))
      (define rhs (car(cdr prd)))
      (or  (is-left-recursive-d head rhs) (is-left-recursive-i head rhs ords)))
    prds))

; Verifica se uma produção é recursiva à esquerda  de forma direta
(define (is-left-recursive-d term rhs)
  (ormap
    (lambda (seq)
      (if (eq? seq '()) #f
        (if (equal? (car seq) term) #t #f)))
    rhs))

; Verifica se uma produção possui possibilidade de recursão à esquerda indireta
(define (is-left-recursive-i  head rhs ords)
  (define pre-ord (before-list ords head ))
  (if (eq? pre-ord '()) #f
    (ormap
      (lambda (seq)
          (if (eq? seq '()) #f
            (ormap
              (lambda (term)                
                (define head-ord (car term))
                (if (equal? head-ord (car seq)) #t #f))
              pre-ord)
          ))
      rhs)))

; Verifica se um elemento está contido em uma lista, se sim, retorna os elementos anteriores a ele 
(define (before-list list element)
  (if (member element list)
    (before-list-aux list element)
    '()))

; Verifica se um elemento está contido em uma lista
(define (member element list)
  (ormap
    (lambda (el)
      (if (equal? (car el) element) #t #f))
    list))

; Retorna os elementos de uma lista anteriores a um elemento
(define (before-list-aux list element)
  (cond
    [(null? list) '()]
    [(equal? (car (car list)) element) '()] 
    [else (cons (car list) (before-list-aux (cdr list) element))]))

; Verifica se a gramática se termos aceitos por gf são aceitos por gs
(define (accepts? gf gs)
    (define g-first  (format-input(car (cdr gf))))
    (define g-second (format-input(car (cdr gs))))

    ; Gera os termos de gf
    (define terms (sample (gen:word-from-grammar g-first) num-words))
    (ormap
      (lambda (term)
        (if (and (not (equal? term '∅)) (not (equal? term #f)))
          (if (in-grammar? g-second term) #t #f)
          #t)) 
      terms))

; Verifica se a gramática gs não aceita termos que não são aceitos por gf   
(define (not-accepts? gf gs)
    (define g-first  (format-input(car (cdr gf))))
    (define g-second (format-input(car (cdr gs))))

    ; Gera os termos de gf
    (define terms (sample (gen:word-from-grammar g-first) num-words))
    (ormap
      (lambda (term)
        (if (and (not (equal? term '∅)) (not (equal? term #f)))
           (if (in-grammar? g-second (cons 0 (append term '(0)))) #t #f)
          #t)) 
      terms))


; Imprime a gramática
(define (print-grammar grammar)
  (displayln "(")
  (let loop ((n 0))
    (if (< n (length grammar))
      (begin
        (displayln (list-ref grammar n))
        (loop (+ n 1)))
      (displayln ")"))))