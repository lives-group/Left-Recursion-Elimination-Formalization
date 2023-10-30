#lang racket
(require redex)

; Definição da gramática
(define-language G
  [nonterminal V]
  [terminal number]
  [rhs (seq ...)]
  [seq (t ...)]
  [order nonterminal]
  [t nonterminal terminal]
  [production (nonterminal rhs)]
  [V variable-not-otherwise-mentioned]
  [productions (production ...)]
  [grammar ((order ...) productions)])

; Redução que transforma recursão à esquerda indireta em recursão à esquerda direta 
(define i-->
  (reduction-relation G
      (-->
        [((name n0 nonterminal_!_1) ... nonterminal_0 (name n1 nonterminal_!_1) ... nonterminal order_3 ...)(production_1 ... (nonterminal_0 ((t ...) ...)) production_2 ...  (nonterminal (seq_0 ... (nonterminal_0  t_1 ...) ((name n2 nonterminal_!_1) t_2 ...) ...)) production ...)]
        [(n0 ... nonterminal_0 n1 ... nonterminal order_3 ...)(production_1 ... (nonterminal_0 ((t ...) ...)) production_2 ... (nonterminal (seq_0 ... (t ... t_1 ...) ... (n2 t_2 ...) ...)) production ...)])

    (-->
     [(order ...) (production ... (nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... )) production_1 ...)] 
     [(order ...) (concat-productions (concat-productions (production ...) (eliminate-left-recursion (new-production nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... )) ))(production_1 ...))])
  ))

; Função que cria uma novo não terminal que produz o vazio
(define-metafunction G
  new-production : nonterminal rhs -> productions
  [(new-production nonterminal ((terminal t ...) ...  (nonterminal_1 t_2 ...) ... (nonterminal t_1 ...) seq_2 ... )) 
   ((nonterminal_new (()))(nonterminal ((terminal t ...) ... (nonterminal_1 t_2 ...) ... (nonterminal t_1 ...) seq_2 ... )))
   (where nonterminal_new ,(variable-not-in (term nonterminal) (term nonterminal) ))])

; Função que elimina a recursão à esquerda direta
(define-metafunction G
  eliminate-left-recursion : productions -> productions

  [(eliminate-left-recursion ((nonterminal_new ((t_0 ...) ...)) (nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... ))))
   (eliminate-left-recursion ((nonterminal_new  ((t_0 ...) ... (t_1 ... nonterminal_new))) (nonterminal ((terminal t ...) ... seq_2 ... ))))]

  [(eliminate-left-recursion ((nonterminal_new ((t_0 ...) ...)) (nonterminal ()))) 
   ((nonterminal_new ((t_0 ...) ...)))]

  [(eliminate-left-recursion ((nonterminal_new ((t_0 ...) ...)) (nonterminal ((terminal t ...) ... (nonterminal_1 t_2 ...) ...))))
   ((nonterminal_new ((t_0 ...) ...)) (nonterminal ((terminal t ... nonterminal_new) ... (nonterminal_1 t_2 ... nonterminal_new) ... )))])

; Função que unifica duas gramaticas
(define-metafunction G
  concat-productions : productions productions -> productions
  [(concat-productions (production_1 ... ) (production_2 ... )) 
   (production_1 ... production_2 ... )])

; Função que ordena a gramática
(define (order-rhs productions)
  (define nonterminals (remove-duplicates (map car productions)))
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

(traces d--> ordered-productions)
;(traces d--> (apply-reduction-relation i--> ordered-productions))