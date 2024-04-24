#lang racket

(provide (all-defined-out))

(require redex
         "struct.rkt")

; Parâmetros de entrada
(define max-terminals 5)
(define min-terminals 3)
(define max-nonterminals 4) ; O valor máximo de não-terminais é 26, pois usamos letras do alfabeto
(define min-nonterminals 2)

(define max-rhs 3)
(define max-seq 5)

; Gera os terminais
(define (generate-terminals size)
  (let loop ((n 1) (acc '()))
    (if (> n size)
      acc
      (loop (+ n 1) (cons n acc)))))

; Gera os não-terminais
(define (generate-nonterminals size)
  (let loop ((n 1) (acc '()))
    (if (> n size)
      acc
        (loop (+ n 1) (cons (list-ref '(S A B C D E F G H I J K L M N O P Q R T U V W X Y Z) (- n 1)) acc)))))

; Sorteia o tamanho do rhs
(define (get-rhs-size)
  (+ 1 (random max-rhs)))

; Sorteia o tamanho da sequência
(define (get-seq-size)
  (+ 1 (random max-seq)))

; Sorteia um número entre 1 e 3 
; (1 = recursão direta, 2 = chance de recursão indireta)
(define (get-seq-type)
  (+ 1 (random 2)))

; Sorteia um terminal ou não-terminal
(define (get-term terminals nonterminals)
    (if (< (random 2) 1)
      (list-ref terminals (random (length terminals)))
      (list-ref nonterminals (random (length nonterminals)))))

; Sorteia um nonteminal que esteja depois do não-terminal passado como parâmetro na lista de não-terminais
(define (sort-nonterminal nonterminal nonterminals)
  (define index (position nonterminal nonterminals 0))
  (if (zero? index)
    (random-elt nonterminals)
    (random-elt (drop nonterminals index))))

; Sorteia um item da lista
(define (random-elt lst)
  (list-ref lst (random (length lst))))

; Retorna o índice do item na lista
(define (position item list count)
   (if (equal? item (car list))
      count
      (position item (cdr list) (+ count 1))))

; Gera uma gramática
(define (generate-grammar terminals nonterminals)
  (let loop ((n 0) (acc '()))
    (if (= n (length nonterminals))
      acc
      (loop (+ n 1) (cons (generate-production (list-ref nonterminals n) terminals nonterminals) acc)))))       

; Gera uma produção
(define (generate-production nonterminal terminals nonterminals)
  (define rhs-size
    (if (eq? 'S nonterminal)
      max-rhs
      (get-rhs-size)))
  (list nonterminal 
    (append
      (let loop ((n 0) (acc '()))
        (if (= n rhs-size)
          acc
          (loop (+ n 1) 
            (cons (generate-sequence nonterminal terminals nonterminals) acc))))
    (list (generate-sequence-terminal terminals)))))

; Gera uma sequência de terminais
(define (generate-sequence-terminal terminals)
  (define seq-size (get-seq-size))
  (let loop ((n 0) (acc '()))
    (if (= n seq-size)
      acc
      (loop (+ n 1) 
        (cons (get-term terminals terminals) acc)))))

; Gera uma sequência
(define (generate-sequence nonterminal terminals nonterminals)
  (define seq-size
    (if (eq? 'S nonterminal)
      max-seq
      (get-seq-size)))
  (let loop ((n 0) (acc '()))
    (if (= n seq-size)
      (append acc (list (get-term terminals terminals)))
      (loop (+ n 1) 
        (cons (generate-seq-item nonterminal terminals nonterminals n seq-size) acc)))))

; Gera um item da sequência
(define (generate-seq-item nonterminal terminals nonterminals index seq-size)
  (define seq-type (get-seq-type))
  (if (eq? 'S nonterminal)
    (get-term nonterminals nonterminals)
    (if (= (+ index 1) seq-size)
      (cond ((= seq-type 1) nonterminal)
        ((= seq-type 2) (sort-nonterminal nonterminal nonterminals)))
      (get-term terminals nonterminals))))

