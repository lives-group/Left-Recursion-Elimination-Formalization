#lang racket

(provide (all-defined-out))

(require redex
         rackcheck
          "../cfg-entry-generator/derivative.rkt"
          "../cfg-entry-generator/util/reducers.rkt"
          "../cfg-entry-generator/main.rkt")

(define (format-input input)
    (define (format-symbol sym)
        (cond
            [(number? sym) 
                (T sym)]
            [(symbol? sym) 
                (NT sym)]))
  
    (define (format-alt alts)
        (cond
            [(<= (length alts) 1) 
                (car (map format-sequence alts))]
            [else
                (Alt(car (map format-sequence alts)) (format-alt-helper (cdr alts)))]))

    (define (format-alt-helper alts)
        (cond
            [(null? alts) 
                '()]
            [(null? (cdr alts)) 
                (car (map format-sequence alts))]
            [else 
                (Alt (car (map format-sequence alts)) (format-alt-helper (cdr alts)))]))

    (define (format-sequence seqs)
        (cond
            [(null? seqs) 
                'ε]
            [(<= (length seqs) 1) 
                (car (map format-symbol seqs))]
            [else
                (Seq (car (map format-symbol seqs)) (format-seq-helper (cdr seqs)))]))

    (define (format-seq-helper seqs)
        (cond
            [(null? seqs) 
                '()]
            [(null? (cdr seqs)) 
                (car (map format-symbol seqs))]
            [else 
                (Seq (car (map format-symbol seqs)) (format-seq-helper (cdr seqs)))]))
  
    (define (format-production nt alts)
        (Production (NT nt) (format-alt alts)))
  
    (define formatted-input (map (lambda (x) (apply format-production x)) input))
        `(,@formatted-input))

; --- Testes/Debug ---
(define input
  '(
        (S ((4 A 6 7) (3 5 3)))
        (A ((C S 2) (1)))
        (C ((8 6 2) (1 2)))
    ))

; Imprime a gramática
(define (print-grammar grammar)
  (displayln "(")
  (let loop ((n 0))
    (if (< n (length grammar))
      (begin
        (displayln (list-ref grammar n))
        (loop (+ n 1)))
      (displayln ")"))))

;(define teste (format-input input))
;(define teste2 (reduce-production teste))
;(print-grammar teste)

;(sample (gen:word-from-grammar teste))
