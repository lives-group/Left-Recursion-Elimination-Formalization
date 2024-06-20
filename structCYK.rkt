#lang racket

(provide (all-defined-out))

(require redex     
    "cfggen/main.rkt"
    "struct.rkt")

(define entradaTeste (format-input '(
               (S ((B 2 3) ()))
               (B ((B) (S 2)))
               (C ((A) (3) (4 5) (1) (D 5)))
               )))


(define (format-input-cyk input)
  (define (format-prd prd)
    (match prd
      [(Production (NT nt) alt)
       (let ((formatted-rhs (coalese-alt (rewrite-rhs alt))))
         (map (lambda (rhs) (cons nt (list rhs))) formatted-rhs))]))

  (define (rewrite-rhs rhs)
    (match rhs 
      [(Alt e d) (Alt (rewrite-rhs e) (rewrite-rhs d))]
      [(Seq (T t) d) (cons t (rewrite-rhs d))]
      [(Seq (NT nt) d) (cons nt (rewrite-rhs d))]
      [(NT nt) (list nt)] 
      [(T t) (list t)]
      [#t (list )] ))

  (define (coalese-alt rhs)
    (match rhs 
      [(Alt e d) (append (coalese-alt e) (coalese-alt d))]
      [x (list x)]))
  
  (define (flatten list-of-lists)
    (apply append list-of-lists))

  (define formatted-input (flatten (map format-prd input)))
  formatted-input)


(displayln (format-input-cyk entradaTeste))
