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
        [((name n0 nonterminal_!_1) ... nonterminal_0 (name n1 nonterminal_!_1) ... nonterminal order_0 ...)((nonterminal (seq_0 ... (nonterminal_0  t_1 ...) ((name n2 nonterminal_!_1) t_2 ...) ...)) production ... (nonterminal_0 ((t ...) ...)) production_0 ...)]
        [(n0 ... nonterminal_0 n1 ... nonterminal order_0 ...)(concat-productions (check-left-recursion (order-production nonterminal (seq_0 ... (t ... t_1 ...) ... (n2 t_2 ...) ...))) (production ... (nonterminal_0 ((t ...) ...)) production_0 ...))])

      (-->
        [((name n0 nonterminal_!_0) ... nonterminal order order_0 ...) ((nonterminal ((terminal t ...) ... ((name n1 nonterminal_!_0)  t_1 ...) ...)) production ...) ]
        [(n0 ... nonterminal order order_0 ...)(concat-productions (production ...) (check-left-recursion (nonterminal ((terminal t ...) ... (n1  t_1 ...) ...))))])

      (-->
        [((name n0 nonterminal_!_0) ... nonterminal) ((nonterminal ((terminal t ...) ... ((name n1 nonterminal_!_0)  t_1 ...) ...)) production ...) ]
        [(n0 ... nonterminal)(concat-productions (check-left-recursion (nonterminal ((terminal t ...) ... (n1  t_1 ...) ...))) (production ...))])

      (-->
        [((name n0 nonterminal_!_0) ...) (((name n1 nonterminal_!_0) rhs) production ...) ]
        [(n0 ...)(production ... (n1 rhs))])
  ))

;Função para eliminar recursão à esquerda direta
(define-metafunction G
  check-left-recursion : production -> productions
  [(check-left-recursion (nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... )))
    (eliminate-left-recursion (new-production nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... )))]

  [(check-left-recursion ((name n0 nonterminal_!_0) ((terminal t ...) ... ((name n1 nonterminal_!_0) t_1 ...) ... )))
    ((n0 ((terminal t ...) ... (n1 t_1 ...) ... )))] 
)

;Função que ordena uma produção
(define-metafunction G
  order-production : nonterminal rhs -> production
  [(order-production nonterminal ((terminal t ...) ... (nonterminal_1 t_0 ...) (nonterminal_2 t_1 ...) ... (terminal_0 t_2 ...) seq ...))
   (order-production nonterminal ((terminal t ...) ... (terminal_0 t_2 ...) (nonterminal_1 t_0 ...) (nonterminal_2 t_1 ...) ... seq ...))]

  [(order-production nonterminal ((terminal t ...) ... (nonterminal_0 t_0 ...) ...))
    (nonterminal (concat-rhs ((terminal t ...) ...)  (order-nonterminal nonterminal () ((nonterminal_0 t_0 ...) ...))))]
)

; Função que ordena os nonterminal de um rhs
(define-metafunction G
  order-nonterminal : nonterminal rhs rhs -> rhs
  [(order-nonterminal nonterminal (seq ...)((nonterminal t ...) seq_1 ...))
   (order-nonterminal nonterminal ((nonterminal t ...) seq ...)(seq_1 ...))]
  
  [(order-nonterminal (name n0 nonterminal_!_0) (seq ...)(((name n1 nonterminal_!_0) t ...) seq_1 ...))
   (order-nonterminal n0 (seq ... (n1 t ...))(seq_1 ...))]
  
  [(order-nonterminal nonterminal rhs ())
   rhs])
   
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
   
  [(eliminate-left-recursion ((nonterminal_new ((t_0 ...) ...)) ((name n0 nonterminal_!_0) ((terminal t ...) ... ((name n1 nonterminal_!_0) t_2 ...) ...))))
   ((n0 (add-new-nonterminal-prodution nonterminal_new ((terminal t ...) ... (n1 t_2 ...) ... ))) (nonterminal_new ((t_0 ...) ...)))])

; Função que concatena uma produção com um new-nonterminal
(define-metafunction G
  add-new-nonterminal-prodution : t rhs -> rhs
  [(add-new-nonterminal-prodution t_1 ((t ... t_1) (t_0 ... ) ...))
  ((t ... t_1) (t_0 ... ) ...)]

  [(add-new-nonterminal-prodution (name t0 t_!_0) ((t_0 ... (name t1 t_!_0)) (t_1 ...) ...))
  ((t_0 ... t1 t0) (t_1 ... t0)  ...)])

; Função que unifica duas gramaticas
(define-metafunction G
  concat-productions : productions productions -> productions
  [(concat-productions (production_1 ... ) (production_2 ... )) 
   (production_1 ... production_2 ... )])

; Função que unifica dois rhs
(define-metafunction G
  concat-rhs : rhs rhs -> rhs
  [(concat-rhs (seq ...) (seq_0 ...))
   (seq ... seq_0 ...)])

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
               (S ((B 2) (S 4) (2)))
               (C ((A) (7 2)))
               (B ((S 2) (B 3)))
               (A ((C A) (S 2)))
               (B ((A) (7 2)))
               ))))

(traces i--> ordered-productions)
;(traces d--> (car (apply-reduction-relation* i--> ordered-productions)))

;;;;; Correções
; corrigir a ordenação para funcionar a linha 126.
; arrumar a redução indireta para substituir o S.

;;;;; Adições
; 1
; fazer codigo (Racket Check) para gerar gramaticas recursivas a esquerda. 
  ; 1 verificar se gramatica não é recursiva a esquerda (direta e indireta)
  ; 2 Gerar insumos para o teste
  ; 3 Vericar a equivalencia da linguagem gerada pela a gramatica original e a gramatica resultante (Thiago)
    ; - Teste baseado em propriedades
    ; - Cobertura de código
    ; - VAI SER O ULTIMO A SER FEITO
; 2
; Moore
; A -> abcBD | abBBA
; A -> abA'
; A' -> cBD | BBA