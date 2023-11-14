#lang racket
(require redex)

; Definição da gramática
(define-language G
  [nonterminal V]
  [terminal number]
  [rhs (seq ...)]
  [seq (t ...)]
  [order (nonterminal flag)]
  [flag 0 1]
  [t nonterminal terminal]
  [production (nonterminal rhs)]
  [V variable-not-otherwise-mentioned]
  [productions (production ...)]
  [orders (order ...)]
  [grammar (orders productions)])

; Redução que elimina recursão à esquerda
(define i-->
  (reduction-relation G
      ;Caso base 
      (-->
        [((nonterminal 0) (nonterminal_1 0) ...) ((nonterminal ((terminal t ...) ... (nonterminal_2  t_1 ...) ...)) production ...) ]

        [((nonterminal 1) (nonterminal_1 0) ...)
        (concat-productions (check-left-recursion (nonterminal ((terminal t ...) ... (nonterminal_2 t_1 ...) ...)) (production ...)) (production ...))])

      ;Caso que tem chance de recursão indireta
      (-->
        [(((name n0 nonterminal_!_1) 1) ... (nonterminal_0 1) ((name n1 nonterminal_!_1) 1) ... (nonterminal 0) order_0 ...)(production ... (nonterminal_0 ((t ...) ...)) production_0 ... (nonterminal (seq_0 ... (nonterminal_0  t_1 ...) ((name n2 nonterminal_!_1) t_2 ...) ...)) production_1 ... )]

        [((n0 1) ... (nonterminal_0 1) (n1 1) ... (nonterminal 0) order_0 ...)(concat-productions (concat-productions (production ... (nonterminal_0 ((t ...) ...)) production_0 ...) (check-left-recursion (order-production nonterminal (seq_0 ... (t ... t_1 ...) ... (n2 t_2 ...) ...)) (concat-productions (production_1 ... ) (production ... (nonterminal_0 ((t ...) ...)) production_0 ...)))) (production_1 ...))])

      ;Caso que tem chance de recursão direta
      (-->
        [(((name n0 nonterminal_!_0) 1) ... (nonterminal 0) order_0 ...) (production_0 ...(nonterminal ((terminal t ...) ... ((name n1 nonterminal_!_0)  t_1 ...) ...)) production ...) ]

        [((n0 1) ... (nonterminal 1) order_0 ...)
        (concat-productions (production_0 ...) (concat-productions (check-left-recursion (nonterminal ((terminal t ...) ... (n1  t_1 ...) ...)) (concat-productions (production_0 ...)(production ...))) (production ...) ))])
  ))

;Função para eliminar recursão à esquerda direta
(define-metafunction G
  check-left-recursion : production productions -> productions
  [(check-left-recursion (nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... )) productions)
    (eliminate-left-recursion (new-production nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... ) (get-list () productions)))]

  [(check-left-recursion (nonterminal ((terminal t ...) ... (nonterminal_0 t_1 ...) ... )) productions)
    ((nonterminal ((terminal t ...) ... (nonterminal_0 t_1 ...) ... )))])

;Função que cria uma lista de nonterminais
(define-metafunction G
  get-list : seq productions  -> seq
  [(get-list (nonterminal ...) ((nonterminal_0 rhs_0) (nonterminal_1 rhs_1)...)) (get-list (nonterminal ... nonterminal_0) ((nonterminal_1 rhs_1)...))]
  [(get-list ( nonterminal ...) ())(nonterminal ...)])

; Função que elimina a recursão à esquerda direta
(define-metafunction G
  eliminate-left-recursion : productions -> productions

  [(eliminate-left-recursion ((nonterminal_new ((t_0 ...) ...)) (nonterminal ((terminal t ...) ... (nonterminal t_1 ...) seq_2 ... ))))
   (eliminate-left-recursion ((nonterminal_new  ((t_0 ...) ... (t_1 ... nonterminal_new))) (nonterminal ((terminal t ...) ... seq_2 ... ))))]

  [(eliminate-left-recursion ((nonterminal_new ((t_0 ...) ...)) (nonterminal ()))) 
   ((nonterminal_new ((t_0 ...) ...)))]
   
  [(eliminate-left-recursion ((nonterminal_new ((t_0 ...) ...)) (nonterminal ((terminal t ...) ... (nonterminal_1 t_2 ...) ...))))
   ((nonterminal_new ((t_0 ...) ...)) (nonterminal ((terminal t ... nonterminal_new) ... (nonterminal_1 t_2 ... nonterminal_new) ... )))])

; Função que cria uma novo não terminal que produz o vazio
(define-metafunction G
  new-production : nonterminal rhs seq -> productions
  [(new-production nonterminal ((terminal t ...) ...  (nonterminal_1 t_2 ...) ... (nonterminal t_1 ...) seq_2 ... ) (nonterminal_5 ...)) 
   ((nonterminal_new (()))(nonterminal ((terminal t ...) ... (nonterminal_1 t_2 ...) ... (nonterminal t_1 ...) seq_2 ... )))
   (where nonterminal_new ,(variable-not-in (term (nonterminal nonterminal_5 ...)) (term nonterminal) ))])

;Função que ordena uma produção
(define-metafunction G
  order-production : nonterminal rhs -> production
  [(order-production nonterminal ((terminal t ...) ... (nonterminal_1 t_0 ...) (nonterminal_2 t_1 ...) ... (terminal_0 t_2 ...) seq ...))
   (order-production nonterminal ((terminal t ...) ... (terminal_0 t_2 ...) (nonterminal_1 t_0 ...) (nonterminal_2 t_1 ...) ... seq ...))]

  [(order-production nonterminal ((terminal t ...) ... (nonterminal_0 t_0 ...) ...))
    (nonterminal (concat-rhs ((terminal t ...) ...)  (order-nonterminal nonterminal () ((nonterminal_0 t_0 ...) ...))))])

; Função que ordena os nonterminal de um rhs
(define-metafunction G
  order-nonterminal : nonterminal rhs rhs -> rhs
  [(order-nonterminal nonterminal (seq ...)((nonterminal t ...) seq_1 ...))
   (order-nonterminal nonterminal ((nonterminal t ...) seq ...)(seq_1 ...))]
  
  [(order-nonterminal (name n0 nonterminal_!_0) (seq ...)(((name n1 nonterminal_!_0) t ...) seq_1 ...))
   (order-nonterminal n0 (seq ... (n1 t ...))(seq_1 ...))]
  
  [(order-nonterminal nonterminal rhs ()) rhs])
   
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
  (define nonterminals (add-flag (remove-duplicates (map car productions))))
  (list nonterminals
    (map
      (lambda (p)
        (define rhs (car (cdr p)))
        (define head (car p))

        (let ((terminal (filter (lambda (x) (or (number? (car x)) (equal? (car x) '()))) rhs))
              (nonterminal (filter (lambda (x) (and (not (number? (car x))) (not (equal? (car x) '())) (not (equal? (car x) head)))) rhs))
              (recursion (filter (lambda (x) (and (not (number? (car x))) (not (equal? (car x) '())) (equal? (car x) head))) rhs)))
          (cons (car p) (list (append terminal recursion nonterminal)))))
    productions)))

; Função que adiciona um flag 0 em cada não terminal ou seja (A 0)
(define (add-flag nonterminals)
  (map (lambda (x) (list x 0)) nonterminals))
  
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
               (S ((B 2) (A 4) (2)))
               (C ((A) (7 2)))
               (B ((S 2) (B 3)))
               (A ((C A) (S 2)))
               (B ((A) (7 2)))
               ))))

(traces i--> ordered-productions)

;;;;; Adições

; 1
; fazer codigo (Racket Check) para gerar gramaticas recursivas a esquerda. 
  ; 1 verificar se gramatica não é recursiva a esquerda (direta e indireta)
  ; 2 Gerar insumos para o teste (usando racket puro, olhar repositorio)
  ; 3 Vericar a equivalencia da linguagem gerada pela a gramatica original e a gramatica resultante (Thiago)
    ; - Teste baseado em propriedades
    ; - Cobertura de código
    ; - VAI SER O ULTIMO A SER FEITO

; 2 (Se der tempo)
; Robert Moore
; A -> abcBD | abBBA
; A -> abA'
; A' -> cBD | BBA