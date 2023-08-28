
#lang racket
(require redex)

; Definição da gramática
(define-language G
    [nonterminal V]
    [terminal number]
    [rhs (seq ...)]
    [seq (t ...)]
    [t nonterminal terminal]
    [production (nonterminal rhs )]
    [V variable-not-otherwise-mentioned]
    [grammar (production ...)])

; Remove a recursão à esquerda da gramática (direta)
(define (elimination-left-recursion productions)
    (displayln "Gramática original:")
    (display-format-productions productions)
    (define new-productions '()) ; Lista de novas produções
    (map
        (lambda (p)
            (define head (car p))
            (define rhs (car(cdr p)))

            ; Cria um novo não-terminal head-new
            (define new-nt (add-suffix head '-new)) 

            (if (is-left-recursive head rhs)
               (begin
                    ; A' -> ε
                    (set! new-productions (add-production new-productions (list new-nt (list '()))))

                    (map
                        (lambda (seq)
                            (define seq-head (car seq))
                            (define seq-tail (cdr seq))
                            (if (equal? head seq-head)
                                ; A -> Aα se torna A -> αA'
                                (when (not (null? seq-tail))
                                    (set! new-productions (add-production new-productions (list head (list (append seq-tail (list new-nt)))))))
                                ; A -> β se torna A' -> βA'
                                (set! new-productions (add-production new-productions (list new-nt (list (append seq (list new-nt))))))
                            )
                        )
                        rhs)
                )
                (set! new-productions (add-production new-productions p))
            )
        )
        productions)
    
    (displayln "")
    (displayln "Gramática sem recursão à esquerda:")
    (define combined-productions (combine-productions new-productions))
    ; Verifica se new-productions é vazia
    (if (null? new-productions)
        (display-format-productions productions)
        (display-format-productions combined-productions)))

; Verifica se uma produção é recursiva à esquerda
(define (is-left-recursive nt rhs)
    (ormap
        (lambda (seq)
            (define head (car seq))
            (if (equal? head nt) #t #f))
        rhs))

; Remove uma produção da gramática
(define (remove-production productions production)
  (filter (lambda (p) (not (equal? p production))) productions))

; Adiciona uma nova produção na gramática
(define (add-production productions new-production)
  (cons new-production productions))

; Adiciona sufixo ao nome do não-terminal
(define (add-suffix nt suffix)
    (string->symbol (string-append (symbol->string nt) (symbol->string suffix))))

; Juntar produções de um mesmo não-terminal
(define (combine-productions productions)
  (define grouped-productions
    (foldl
        (lambda (p acc)
            (let* ((head (car p))
                    (rhs (car(cdr p)))
                    (existing-pair (assoc head acc)))
                (if existing-pair
                    (cons (cons head (list (append (car(cdr existing-pair)) rhs))) (remove existing-pair acc))
                    (cons (cons head (list rhs)) acc))))
        '() productions))
  
  (map (lambda (pair)
        (cons (car pair) (cdr pair)))
       grouped-productions))

; Imprime a gramática
(define (display-format-productions productions)
  (for-each
   (lambda (p)
        (define head (car p))
        (define rhs (car (cdr p)))
        (define formatted-rhs
        (map (lambda (seq)
            (if (null? seq)
                "ε"
                (string-join (map to-string seq) " "))) rhs)
            )
            (display (format "~a -> ~a\n"
                (symbol->string head)
                (string-join formatted-rhs " | "))))
        productions))

; Converte para string
(define (to-string value)
  (if (number? value)
      (number->string value)
      (symbol->string value)))

; Testes

; S -> A 2
; A -> 2 3 | 4
#;(redex-match G grammar (term (
                              (S ((A 2)))
                              (A ((2 3) (4)))
                              )))
; S -> S 2
#;(redex-match G ((nonterminal (seq ... (nonterminal_1 t ...) seq_1 ... ) )) (term (
                              (S ((S 2)))
                              )))
; S -> A 2 | 4
; A -> A 3 | B
; B -> B | 2
(elimination-left-recursion '(
                   (S ((A 2) (4)))
                   (A ((A 3) (B)))
                   (B ((B) (2)))
                   ))