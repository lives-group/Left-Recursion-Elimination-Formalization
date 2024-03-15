#lang racket

(provide (all-defined-out))

(define (format-input input)
    (define (format-symbol sym)
        (cond
            [(number? sym) 
                `(T ,sym)]
            [(symbol? sym) 
                `(NT ',sym)]))
  
    (define (format-alt alts)
        (cond
            [(<= (length alts) 1) 
                (car (map format-sequence alts))]
            [else
                `(Alt ,(car (map format-sequence alts)) ,(format-alt-helper (cdr alts)))]))

    (define (format-alt-helper alts)
        (cond
            [(null? alts) 
                '()]
            [(null? (cdr alts)) 
                (car (map format-sequence alts))]
            [else 
                `(Alt ,(car (map format-sequence alts)) ,(format-alt-helper (cdr alts)))]))

    (define (format-sequence seqs)
        (cond
            [(null? seqs) 
                'ε]
            [(<= (length seqs) 1) 
                (car (map format-symbol seqs))]
            [else
                `(Seq ,(car (map format-symbol seqs)) ,(format-seq-helper (cdr seqs)))]))

    (define (format-seq-helper seqs)
        (cond
            [(null? seqs) 
                '()]
            [(null? (cdr seqs)) 
                (car (map format-symbol seqs))]
            [else 
                `(Seq ,(car (map format-symbol seqs)) ,(format-seq-helper (cdr seqs)))]))
  
    (define (format-production nt alts)
        `(Production (NT ',nt) ,(format-alt alts)))
  
    (define formatted-input (map (lambda (x) (apply format-production x)) input))
        `(,@formatted-input))

; --- Testes/Debug ---
(define input
  '(
        (S ((4 A 6 7) (C S) (S 3 5 3)))
        (A ((C S S 2)))
        (B ((1 B) (4 6) (C S 5 A)))
        (C ((S) (8 6 2) (C 2)))
    ))

;(print-grammar (format-input input))
