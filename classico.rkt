#lang racket
(require redex)

; Definição da gramática
(define-language G
  [nonterminal V]
  [terminal number ε]
  [rhs (seq ...)]
  [seq (t ...)]
  [t nonterminal terminal]
  [production (nonterminal rhs)]
  [V variable-not-otherwise-mentioned]
  [grammar (production ...)])

; Redução
(define g-->
  (reduction-relation G
                      (--> [(nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... )) production_1 ...] 
                           [concat-grammar (production_1 ...) (left-recursion-elimination nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... ))] "A -> Aα")
                        
                      (--> [(nonterminal ((terminal t ...) ... (nonterminal_1 t ...) ... ) production_1 ...)] 
                           [production_1 ... (nonterminal ((terminal t ...) ... (nonterminal_1 t ...) ... ))])
  ))

; Função que elimina a recursão à esquerda direta
(define-metafunction G
  left-recursion-elimination : nonterminal rhs -> grammar

  [(left-recursion-elimination nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... )) 
   ((nonterminal_new ((ε)))(nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... )))
   (where nonterminal_new ,(variable-not-in (term nonterminal) (term nonterminal) ))]
)

;-------------------- Funções auxiliares --------------------

; Função que unifica duas gramaticas
(define-metafunction G
  concat-grammar : grammar grammar -> grammar

  [(concat-grammar (production_1 ... ) (production_2 ... )) 
   (production_1 ... production_2 ... )]
)

; Função que ordena a gramática
(define (order-rhs productions)
  (map
   (lambda (p)
     (define rhs (car(cdr p)))
            
     (let ((terminal (filter (lambda (x) (or (number? (car x)) (equal? (car x) 'ε))) rhs))  
           (nonterminal (filter (lambda (x) (not (or (number? (car x)) (equal? (car x) 'ε)))) rhs))) 
       (cons (car p) (list(append terminal nonterminal))))
     )
  productions))

;-------------------- Testes --------------------
(define ordered-productions
  (order-rhs '(
               (S ((S 2) (4) (2)))
               (A ((A 3) (B 1) (A B)))
               (B ((B) (2)))
               )))

(traces g--> ordered-productions)