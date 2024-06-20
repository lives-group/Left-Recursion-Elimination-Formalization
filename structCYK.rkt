#lang racket

(provide (all-defined-out))

(require redex     
    "cfggen/main.rkt"
    "struct.rkt")

(define entradaTeste (format-input '(
               (S ((B 2 3) (5 3)))
               (B ((B) (S 2 1 2)))
               (C ((A) (3 4) (4 5 5) (1) (D 5)))
               )))

(define (format-input-cyk input)
  (define counter 1)
  
  (define (generate-new-nt nt)
    (let ((new-nt (string->symbol (string-append "AUX" (number->string counter)))))
      (set! counter (+ counter 1))
      new-nt))
  
  (define (format-prd prd)
    (match prd
      [(Production (NT nt) alt)
       (let ((formatted-rhs (coalese-alt (rewrite-rhs alt))))
         (flatten (map (lambda (rhs) (process-rhs nt rhs)) formatted-rhs)))]))

  (define (rewrite-rhs rhs)
    (match rhs 
      [(Alt e d) (Alt (rewrite-rhs e) (rewrite-rhs d))]
      [(Seq (T t) d) (cons t (rewrite-rhs d))]
      [(Seq (NT nt) d) (cons nt (rewrite-rhs d))]
      [(NT nt) (list nt)] 
      [(T t) (list t)]
      [#t (list)]))

  (define (coalese-alt rhs)
    (match rhs 
      [(Alt e d) (append (coalese-alt e) (coalese-alt d))]
      [x (list x)]))
  
  (define (process-rhs nt rhs)
    (if (<= (length rhs) 2)
        (list (cons nt (list rhs)))
        (let ((new-nt (generate-new-nt nt)))
          (append (list (list nt (list (car rhs) new-nt)))
                  (process-rhs new-nt (cdr rhs))))))

  (define (flatten list-of-lists)
    (apply append list-of-lists))

  (define formatted-input (flatten (map format-prd input)))
  formatted-input)

(displayln (format-input-cyk entradaTeste))