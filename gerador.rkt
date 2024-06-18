#lang racket

(provide (all-defined-out))

(require redex
         "struct.rkt")

; Parâmetros de entrada
(define max-trms 3)
(define min-trms 2)
(define max-nts 3) ; O valor máximo de não-terminais é 26, pois usamos letras do alfabeto
(define min-nts 2)

(define max-rhs 3) ; Deve ser maior que 1
(define max-seq 3) ; Deve ser maior que 1

; Gera os terminais
(define (generate-trms size)
  (let loop ((n 1) (acc '()))
    (if (> n size)
      acc
      (loop (+ n 1) (cons n acc)))))

; Gera os não-terminais
(define (generate-nts size)
  (let loop ((n 1) (acc '()))
    (if (> n size)
      acc
        (loop (+ n 1) (cons (list-ref '(S A B C D E F G H I J K L M N O P Q R T U V W X Y Z) (- n 1)) acc)))))

; Sorteia o tamanho do rhs
(define (get-rhs-size)
  (+ 1 (random (- max-rhs 1))))

; Sorteia o tamanho da sequência
(define (get-seq-size)
  (+ 1 (random (- max-seq 1))))

; Sorteia um número entre 1 e 3 
; (1 = recursão direta, 2 = chance de recursão indireta)
(define (get-seq-type)
  (+ 1 (random 2)))

; Sorteia um trm ou não-trm
(define (get-term trms nts)
    (if (< (random 2) 1)
      (list-ref trms (random (length trms)))
      (list-ref nts (random (length nts)))))

; Sorteia um nonteminal que esteja depois do não-trm passado como parâmetro na lista de não-terminais
(define (sort-nt nt nts)
  (define index (position nt nts 0))
  (if (zero? index)
    (random-elt nts)
    (random-elt (drop nts index))))

; Sorteia um item da lista
(define (random-elt lst)
  (list-ref lst (random (length lst))))

; Retorna o índice do item na lista
(define (position item list count)
   (if (equal? item (car list))
      count
      (position item (cdr list) (+ count 1))))

; Gera uma gramática
(define (generate-grammar trms nts)
  (let loop ((n 0) (acc '()))
    (if (= n (length nts))
      acc
      (loop (+ n 1) (cons (generate-prd (list-ref nts n) trms nts) acc)))))       

; Gera uma produção
(define (generate-prd nt trms nts)
  (define rhs-size
    (if (eq? 'S nt)
      (- max-rhs 1)
      (get-rhs-size)))
  (list nt 
    (append
      (let loop ((n 0) (acc '()))
        (if (= n rhs-size)
          acc
          (loop (+ n 1) 
            (cons (generate-sequence nt trms nts) acc))))
    (list (generate-sequence-trm trms)))))

; Gera uma sequência de terminais
(define (generate-sequence-trm trms)
  (define seq-size (get-seq-size))
  (let loop ((n 0) (acc '()))
    (if (= n seq-size)
      acc
      (loop (+ n 1) 
        (cons (get-term trms trms) acc)))))

; Gera uma sequência
(define (generate-sequence nt trms nts)
  (define seq-size
    (if (eq? 'S nt)
      (- max-seq 1)
      (get-seq-size)))
  (let loop ((n 0) (acc '()))
    (if (= n seq-size)
      (append acc (list (get-term trms trms)))
      (loop (+ n 1) 
        (cons (generate-seq-item nt trms nts n seq-size) acc)))))

; Gera um item da sequência
(define (generate-seq-item nt trms nts index seq-size)
  (define seq-type (get-seq-type))
  (if (eq? 'S nt)
    (get-term nts nts)
    (if (= (+ index 1) seq-size)
      (cond ((= seq-type 1) nt)
        ((= seq-type 2) (sort-nt nt nts)))
      (get-term trms nts))))

