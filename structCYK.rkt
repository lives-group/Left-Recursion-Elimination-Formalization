#lang racket

(provide (all-defined-out))

(require redex     
    "cfggen/main.rkt"
    "struct.rkt")

(define entradaTeste (format-input '(
               (S ((B 2 3)))
               (B ((B) (S 2)))
               (C ((A) (3) (4 5) (1) (D 5)))
               )))

(struct prod (nt rhs) #:transparent)

(define (make-pars xs) 
    (match xs 
        ['() '()]
        [(cons x xs) (cons (make-splits x) (make-pars xs))]))

(define (make-splits x)
    (match x
        [(cons nt rhs) (make-splits2 nt rhs)]))

(define (make-splits2 nt rhs)
    (match rhs
        ['() '()]
        [(cons x xs) (cons (prod nt x) (make-splits2 nt xs))]))

(define (format-input-cyk input)
    (define (format-prd prd)
        (match prd
        [(Production (NT nt) alt)
          (cons nt (coalese-alt (rewrite-rhs alt)))]))

    (define (rewrite-rhs rhs)
      (match rhs 
        [(Alt e d) (Alt (rewrite-rhs e) (rewrite-rhs d))]
        [(Seq (T t) d) (cons t (rewrite-rhs d))]
        [(Seq (NT nt) d) (cons nt (rewrite-rhs d))]
        [(NT nt) (list nt)]
        [(T t) (list t)]))
    
    (define (coalese-alt rhs)
      (match rhs 
        [(Alt e d) (list* (car (coalese-alt e)) (coalese-alt d))]
        [x (list x)]))
    
    (define formatted-input (map format-prd input)) formatted-input)

(displayln (make-pars(format-input-cyk entradaTeste)))
