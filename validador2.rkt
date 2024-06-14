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
(define num-tests 50)
; Quatidade de palavras 
(define num-words 10)
; Tamanho máximo de cada palavra 
(define max-wrd-size 10)

; Imprime a gramática
(define (print-grammar grammar)
  (displayln "(")
  (let loop ((n 0))
    (if (< n (length grammar))
      (begin
        (displayln (list-ref grammar n))
        (loop (+ n 1)))
      (displayln ")"))))

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



;---- Funções auxiliares ----
; Verifica se na gramática existe alguma produção recursiva à esquerda
(define (has-left-recursion? grammar)
  (define productions (car (cdr grammar)))
  (define orders (car grammar))
   
  (ormap
    (lambda (prd)
      (define head (car prd))
      (define rhs (car(cdr prd)))
      (or  (is-left-recursive-d head rhs) (is-left-recursive-i head rhs orders)))
    productions))

; Verifica se uma produção é recursiva à esquerda  de forma direta
(define (is-left-recursive-d term rhs)
  (ormap
    (lambda (seq)
      (if (eq? seq '()) #f
        (if (equal? (car seq) term) #t #f)))
    rhs))

; Verifica se uma produção possui possibilidade de recursão à esquerda indireta
(define (is-left-recursive-i  head rhs orders)
  (define pre-order (before-list orders head ))
  (if (eq? pre-order '()) #f
    (ormap
      (lambda (seq)
          (if (eq? seq '()) #f
            (ormap
              (lambda (term)                
                (define head-ord (car term))
                (if (equal? head-ord (car seq)) #t #f))
              pre-order)
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
(define (accepts? gf gs ws)
    (define g-first  (format-input (car (cdr gf))))
    (define g-second (format-input (car (cdr gs))))

    (andmap
      (lambda (term)
        (if (and (not (equal? term '∅)) (not (equal? term #f)))
          (if (and (in-grammar? g-first term) (in-grammar? g-second term))
              #t
              (begin
                (display "non-equivalent word: ")
                (displayln term)
                (display "Gf: ")
                (print-grammar gf)
                (display "Gs: ")
                (print-grammar gs)
                #f))
          #t)) 
      ws))

; Verifica se a gramática gs não aceita termos que não são aceitos por gf   
(define (not-accepts? gf gs ws)
    (define g-first  (format-input(car (cdr gf))))
    (define g-second (format-input(car (cdr gs))))

    ; Gera os termos de gf
    (andmap
      (lambda (term)
        (if (and (not (equal? term '∅)) (not (equal? term #f)))
           (if (in-grammar? g-second (cons 0 (append term '(0)))) #t #f)
          #t)) 
      ws))

(define (Σg g) g ;gramática --> Conjunto de terminais
   (remove-duplicates (filter (lambda (y) (number? y))
                              (car (map (lambda (x) (flatten (cdr x))) (cdr g)))))
  )

(define (gen:random-words Σ maxlen)
   (gen:list (gen:one-of Σ) #:max-length  maxlen)
  )

(define (words-not-in g ws)
   (filter (lambda (w) (not (in-grammar? g w))) ws)
  )

 (check-property (make-config #:tests num-tests
                              #:deadline (* (+ (current-inexact-milliseconds) 3600000) 24))
    (property recursaoRemovida ([g1 gen:grammar-ordered])
      ;Constrói uma lista de palavras aceitas
      (define acc-ws (sample (gen:word-from-grammar (format-input (car (cdr g1)))) num-words))
      ;Constrói uma lista de palavras não aceitas
      (define nacc-ws (filter (lambda (w) (not (in-grammar? (format-input (car (cdr g1))) w)))
                              (sample (gen:random-words (Σg g1) max-wrd-size) num-words) ))    
      ; Aplica a remoção de recursão à esquerda
      (define g2 (car (apply-reduction-relation* i--> g1)))
      ; Verifica se as recursões à esquerda foram removidas
      (check-equal? (has-left-recursion? g2) #f)
      ; Verifica se termos aceitos por g1 são aceitos por g2
      (check-equal? (accepts? g1 g2 acc-ws) #t)
      ; Verifica se os termos NÃO aceitos por g1 são aceitos por g2
      ;(check-equal? (not (accepts? g1 g2 nacc-ws)) #t)
      
      ;(define acc-ws2 (sample (gen:word-from-grammar g2 num-words)))
       ;Constrói uma lista de palavras não aceitas
      ;(define nacc-ws2 (filter (lambda (w) (not (in-grammar? (format-input (car (cdr g2))) w)))
      ;                         (sample (gen:random-words (Σg g2) max-wrd-size) num-words) ))
       ;(check-equal? (accepts? g1 g2 acc-ws2) #t)
       ; Verifica se os termos NÃO aceitos por g1 são aceitos por g2
       ;(check-equal? (not (accepts? g1 g2 nacc-ws2)) #t)
     
      ))


(define (g-word) '(1 2))
(define (gf-test)
   '(((S 0) (A 0))
    ((S ((1 2) (S S A A 5) (S A A A 5))) (A ((5 3 2) (S 2))))
    ))
(define (gs-test)
  '( ((S 1) (A 1))
    ((S ((1 2 S1))) (S1 (() (S A A 5 S1) (A A A 5 S1))) (A ((5 3 2) (1 2 S1 2))))
))