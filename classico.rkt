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
    (-->
     [(nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... )) production_1 ...] 
     [concat-grammar (production_1 ...) (eliminate-left-recursion (new-nonterminal nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... )))] "A -> Aα")

     (-->
      [(nonterminal ((terminal t ...) ... (nonterminal_1 t ...) ... ) production_1 ...)] 
      [production_1 ... (nonterminal ((terminal t ...) ... (nonterminal_1 t ...) ... ))])
     ))

; Função que cria uma novo não terminal que produz ε
(define-metafunction G
  new-nonterminal : nonterminal rhs -> grammar

  [(new-nonterminal nonterminal ((terminal t ...) ...  (nonterminal_1 t_2 ...) ... (nonterminal t_1 ...) seq_2 ... )) 
   ((nonterminal_new ((ε)))(nonterminal ((terminal t ...) ... (nonterminal_1 t_2 ...) ... (nonterminal t_1 ...) seq_2 ... )))
   (where nonterminal_new ,(variable-not-in (term nonterminal) (term nonterminal) ))])

; Função que elimina a recursão à esquerda
(define-metafunction G
  eliminate-left-recursion : grammar -> grammar

  [(eliminate-left-recursion ((nonterminal_new ((t_0 ...) ...)) (nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... ))))
   (eliminate-left-recursion ((nonterminal_new  ((t_0 ...) ... (t_1 ... nonterminal_new))) (nonterminal ((terminal t ...) ... seq_2 ... ))))]

  [(eliminate-left-recursion ((nonterminal_new ((t_0 ...) ...)) (nonterminal ()))) 
   ((nonterminal_new ((t_0 ...) ...)))]

  [(eliminate-left-recursion ((nonterminal_new ((t_0 ...) ...)) (nonterminal ((terminal t ...) ... (nonterminal_1 t_2 ...) ...))))
   ((nonterminal_new ((t_0 ...) ...)) (nonterminal ((terminal t ... nonterminal_new) ... (nonterminal_1 t_2 ... nonterminal_new) ... )))])

; Função que unifica duas gramaticas
(define-metafunction G
  concat-grammar : grammar grammar -> grammar

  [(concat-grammar (production_1 ... ) (production_2 ... )) 
   (production_1 ... production_2 ... )])

; Função que ordena a gramática
(define (order-rhs productions)
  (map
   (lambda (p)
     (define rhs (car (cdr p)))
     (define head (car p))

     (let ((terminal (filter (lambda (x) (or (number? (car x)) (equal? (car x) 'ε))) rhs))
           (nonterminal (filter (lambda (x) (and (not (number? (car x))) (not (equal? (car x) 'ε)) (not (equal? (car x) head)))) rhs))
           (recursion (filter (lambda (x) (and (not (number? (car x))) (not (equal? (car x) 'ε)) (equal? (car x) head))) rhs)))
       (cons (car p) (list (append terminal recursion nonterminal))))
     )
   productions))

; Testes
(define ordered-productions
  (order-rhs '(
               (S ((S 2) (4) (2)))
               (A ((C 1) (A 3) (D 1) (7 B)))
               (B ((B) (B 2)))
               )))

(traces g--> ordered-productions)
