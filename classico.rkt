#lang racket
(require redex)

; Definição da gramática
(define-language G
  [nonterminal V]
  [terminal number]
  [rhs (seq ...)]
  [seq (t ...)]
  [order (nonterminal number)]
  [t nonterminal terminal]
  [production (nonterminal rhs)]
  [V variable-not-otherwise-mentioned]
  [grammar ((order ...) (production ...))])

; Redução que transforma recursão à esquerda indireta em recursão à esquerda direta 
(define i-->
  (reduction-relation G
      (-->
        [(order_1 ... (nonterminal_0 number_0) order_2 ... (nonterminal number_c) order_3 ...)((nonterminal (seq_0 ... (nonterminal_0  t_1 ...) seq_1 ...)) production_1 ... (nonterminal_0 ((t ...) ...)) production_2 ...)] 
        [(order_1 ... (nonterminal_0 number_0)  order_2 ... (nonterminal number_c) order_3 ...)(production_1 ... (nonterminal_0 ((t ...) ...)) production_2 ... (nonterminal (seq_0 ... (t ... t_1 ...) ... seq_1 ...)))])

       (-->
        [((nonterminal_!_0 number_0)... (nonterminal_1 number_c) order_3 ...)((nonterminal_1 ((terminal t_0 ...) ... (nonterminal_!_0  t_1 ...) ...)) production_1 ...)] 
        [((nonterminal_!_0 number_0) ... (nonterminal_1 number_c) order_3 ...)(production_1 ... (nonterminal_1 ((terminal t_0 ...) ... (nonterminal_0  t_1 ...) ...)))] )
  ))
      
;; deixar recursão à esquerda mais generica
;; colocar contexto
;;; nonterminal ::= non_rec non_no_rec
;;; non_rec ::= (nonterminal (seq ... (nonterminal seq_1 ....) seq ...))

;;; v ::= (nonterminal_!_ (nonterminal_!_ seq ...))
;;; C ::= hole (non_rec ... C production ...)
;;; production ::


; corrigir redução
; remover recursões indiretas

; Redução para eliminar recursão à esquerda direta
(define d-->
  (reduction-relation G
    (-->
     [(nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... )) production_1 ...] 
     [concat-grammar (production_1 ...) (eliminate-left-recursion (new-production nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... )))] "A -> Aα")

     (-->
      [(nonterminal ((terminal t ...) ... (nonterminal_1 t ...) ... )) production_1 ...] 
      [production_1 ... (nonterminal ((terminal t ...) ... (nonterminal_1 t ...) ... ))])
     ))

; Função que cria uma novo não terminal que produz o vazio
(define-metafunction G
  new-production : nonterminal rhs -> grammar

  [(new-production nonterminal ((terminal t ...) ...  (nonterminal_1 t_2 ...) ... (nonterminal t_1 ...) seq_2 ... )) 
   ((nonterminal_new (()))(nonterminal ((terminal t ...) ... (nonterminal_1 t_2 ...) ... (nonterminal t_1 ...) seq_2 ... )))
   (where nonterminal_new ,(variable-not-in (term nonterminal) (term nonterminal) ))])

; Função que elimina a recursão à esquerda direta
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

  (define nonterminals (enumerate (remove-duplicates (map car productions))))
  (list nonterminals
    (map
    (lambda (p)
      (define rhs (car (cdr p)))
      (define head (car p))

      (let ((terminal (filter (lambda (x) (or (number? (car x)) (equal? (car x) '()))) rhs))
            (nonterminal (filter (lambda (x) (and (not (number? (car x))) (not (equal? (car x) '())) (not (equal? (car x) head)))) rhs))
            (recursion (filter (lambda (x) (and (not (number? (car x))) (not (equal? (car x) '())) (equal? (car x) head))) rhs)))
        (cons (car p) (list (append terminal recursion nonterminal))))
      )
    productions)
  ) 
)

; Função que remove elementos repetidos de uma lista
(define (remove-duplicates lst)
  (cond
    [(empty? lst) empty]
    [(member (first lst) (rest lst)) (remove-duplicates (rest lst))]
    [else (cons (first lst) (remove-duplicates (rest lst)))]))

; Função que enumera os elementos de uma lista ex: (a b c) -> ((a 1) (b 2) (c 3))
(define (enumerate lst)
  (define (enumerate-aux lst n)
    (cond
      [(empty? lst) empty]
      [else (cons (list (first lst) n) (enumerate-aux (rest lst) (+ n 1)))]))
  (enumerate-aux lst 1))

; Função que unifica produções repetidas
(define (unify-productions productions)
  (define (helper productions result)
    (if (null? productions)
        (reverse result)
        (let* ((current (car productions))
               (key (car current))
               (rest (cdr current))
               (existing (assoc key result)))
          (if existing
              (helper (cdr productions)
                      (cons (cons key (list (concat-list (car (cdr existing)) (car rest)))) (filter (lambda (x) (not (equal? key (car x)))) result)))
              (helper (cdr productions)
                      (cons current result))))))
  (helper productions '()))

; Função que concatena duas listas
(define (concat-list lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (concat-list (cdr lst1) lst2))))

; Testes
(define ordered-productions
  (order-rhs
    (unify-productions '(
               (S ((B 2) (4) (2)))
               (B ((B) (S 2)))
               (A ((F) (S 2)))
               (B ((A) (7 2)))
               ))))

(traces i--> ordered-productions)