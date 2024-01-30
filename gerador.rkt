#lang racket

(require rackcheck)
(provide (all-defined-out))

(require redex
         "classico.rkt")

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
    (let loop ((n 1) (acc '()))
        (if (> n num-nonterminals)
            acc
             (loop (+ n 1) (cons (list-ref '(S A B C D E F G H I J K L M N O P Q R T U V W X Y Z) (- n 1)) acc)))))

; Sorteia o tamanho do rhs
(define (get-rhs-size)
  (+ 1 (random max-rhs)))

; Sorteia o tamanho da sequência
(define (get-seq-size)
  (+ 1 (random max-seq)))

; Sorteia um número entre 1 e 3 
; (1 = recursão direta, 2 = chance de recursão indireta, 3 = sorteio de terminal ou não-terminal)
(define (get-seq-type)
    (+ 1 (random 3)))

; Sorteia um terminal ou não-terminal
(define (get-term terminals nonterminals)
    (if (< (random 2) 1)
        (list-ref terminals (random num-terminals))
        (list-ref nonterminals (random num-nonterminals))))

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
        (if (= n num-nonterminals)
            acc
            (loop (+ n 1) (cons (generate-production (list-ref nonterminals n) terminals nonterminals) acc)))))       

; Gera uma produção
(define (generate-production nonterminal terminals nonterminals)
    (define rhs-size (get-rhs-size))
    (list nonterminal 
    (let loop ((n 0) (acc '()))
        (if (= n rhs-size)
            acc
            (loop (+ n 1) 
              (cons (generate-sequence nonterminal terminals nonterminals) acc))))))

; Gera uma sequência
(define (generate-sequence nonterminal terminals nonterminals)
    (define seq-size (get-seq-size))
    (let loop ((n 0) (acc '()))
        (if (= n seq-size)
            acc
            (loop (+ n 1) 
              (cons (generate-seq-item nonterminal terminals nonterminals n) acc)))))

; Gera um item da sequência
(define (generate-seq-item nonterminal terminals nonterminals index)
  (define seq-type (get-seq-type))
    (if (zero? index)
        (cond ((= seq-type 1) nonterminal)
          ((= seq-type 2) (sort-nonterminal nonterminal nonterminals))
          ((= seq-type 3) (get-term terminals nonterminals)))
      (get-term terminals nonterminals)))


; --- Debug ---

; Imprime a gramática
(define (print-grammar grammar)
  (displayln "(")
  (let loop ((n 0))
    (if (< n (length grammar))
      (begin
        (displayln (list-ref grammar n))
        (loop (+ n 1)))
      (displayln ")"))))
; -------------

(define ordered-productions
  (order-rhs
    (unify-productions (generate-grammar (generate-terminals) (generate-nonterminals)))))

(traces i--> ordered-productions)

;;; (define terminals (generate-terminals))
;;; (define nonterminals (generate-nonterminals))
;;; (print-grammar (generate-grammar terminals nonterminals))