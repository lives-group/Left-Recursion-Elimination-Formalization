#lang racket
(require redex)

(provide (all-defined-out))

; Definição da gramática
(define-language G
  [nt V]
  [trm number]
  [rhs (seq ...)]
  [seq (t ...)]
  [ord (nt flag)]
  [flag 0 1]
  [t nt trm]
  [prd (nt rhs)]
  [V variable-not-otherwise-mentioned]
  [prds (prd ...)]
  [ords (ord ...)]
  [grammar (ords prds)])

; Redução que elimina recursão à esquerda
(define i-->
  (reduction-relation G
      ; Caso base 
      (-->
        [((nt 0) (nt_1 0) ...) 
         ((nt ((trm t ...) ... (nt_2  t_1 ...) ...)) prd ...) ]

        [((nt 1) (nt_1 0) ...)
         (concat-prds 
          (check-left-recursion 
            (nt ((trm t ...) ... (nt_2 t_1 ...) ...)) 
            (prd ...)) 
          (prd ...))] "Caso base")

      ; Caso que tem chance de recursão indireta
      (-->
        [(((name n0 nt_!_1) 1) ... (nt_0 1) ((name n1 nt_!_1) 1) ... (nt 0) ord_0 ...) 
         (prd ... (nt_0 ((t ...) ...)) prd_0 ... (nt ((trm t_0 ... ) ... (nt_2 t_2 ...) ... (nt_0  t_1 ...) seq_1 ...)) prd_1 ... )]

        [((n0 1) ... (nt_0 1) (n1 1) ... (nt 0) ord_0 ...)
         (concat-prds 
            (prd ... (nt_0 ((t ...) ...)) prd_0 ... (ord-prd nt ((trm t_0 ... ) ... (nt_2 t_2 ...) ... (t ... t_1 ...) ... seq_1 ...)))
            (prd_1 ...))] (where 1 (check-difference ((n0 1) ... (nt_0 1) (n1 1) ...) ((nt_2 t_2 ...) ...))) "Caso que tem chance de recursão indireta")

      ; Caso que tem chance de recursão direta
      (-->
        [((nt_0 1) (nt_1 1) ... (nt 0) ord ...)
          (prd_0 ...(nt ((trm t_0 ... ) ... (nt_2 t_1 ...) ...)) prd ...)]
          
        [((nt_0 1) (nt_1 1) ... (nt 1) ord ...)
        (concat-prds 
          (prd_0 ...) 
          (concat-prds 
            (check-left-recursion 
              (nt ((trm t_0 ... ) ... (nt_2 t_1 ...) ...))
              (prd_0 ... prd ...))
            (prd ...) ))]
          (where 1 (check-difference ((nt_0 1) (nt_1 1) ...) ((nt_2 t_1 ...) ...))) "Caso que tem chance de recursão direta")
  ))

; Função que verifica se não há nenhum nt de ord contido no primeiro termo de um rhs
(define-metafunction G
  check-difference : ords rhs -> flag
  [(check-difference (((name n0 nt_!_1) _) ...) ((nt_!_1 t ...) (nt_2 t_2 ...) ...))
   (check-difference ((n0 1) ...) ((nt_2 t_2 ...) ...))]

  [(check-difference ords ()) 1]

  [(check-difference (ord ... (nt flag_1) ord_1 ... ) (seq ... (nt t ...) seq_1 ... )) 0])

; Função para eliminar recursão à esquerda direta
(define-metafunction G
  check-left-recursion : prd prds -> prds
  [(check-left-recursion (nt ((trm t ...) ... (nt t_1 ...) seq_2 ... )) prds)
    (eliminate-left-recursion (new-prd nt ((trm t ...) ... (nt t_1 ...) seq_2 ... ) (get-list () prds)))]

  [(check-left-recursion (nt ((trm t ...) ... (nt_0 t_1 ...) ... )) prds)
    ((nt ((trm t ...) ... (nt_0 t_1 ...) ... )))])

; Função que cria uma lista de nonterminais
(define-metafunction G
  get-list : seq prds  -> seq
  [(get-list (nt ...) ((nt_0 rhs_0) (nt_1 rhs_1)...)) (get-list (nt ... nt_0) ((nt_1 rhs_1)...))]
  [(get-list ( nt ...) ())(nt ...)])

; Função que elimina a recursão à esquerda direta
(define-metafunction G
  eliminate-left-recursion : prds -> prds

  [(eliminate-left-recursion ((nt_new ((t_0 ...) ...)) (nt ((trm t ...) ... (nt t_1 t_2 ...) seq_2 ... ))))
   (eliminate-left-recursion ((nt_new  ((t_0 ...) ... (t_1 t_2 ... nt_new))) (nt ((trm t ...) ... seq_2 ... ))))]

  [(eliminate-left-recursion ((nt_new ((t_0 ...) ...)) (nt ((trm t ...) ... (nt) seq_2 ... ))))
   (eliminate-left-recursion ((nt_new  ((t_0 ...) ...)) (nt ((trm t ...) ... seq_2 ... ))))]

  [(eliminate-left-recursion ((nt_new ((t_0 ...) ...)) (nt ((trm t ...) ... (nt_1 t_2 ...) ...))))
   ((nt_new ((t_0 ...) ...)) (nt ((trm t ... nt_new) ... (nt_1 t_2 ... nt_new) ... )))])

; Função que cria uma novo não trm que produz o vazio
(define-metafunction G
  new-prd : nt rhs seq -> prds
  [(new-prd nt ((trm t ...) ...  (nt_1 t_2 ...) ... (nt t_1 ...) seq_2 ... ) (nt_5 ...)) 
   ((nt_new (()))(nt ((trm t ...) ... (nt_1 t_2 ...) ... (nt t_1 ...) seq_2 ... )))
   (where nt_new ,(variable-not-in (term (nt nt_5 ...)) (term nt) ))])

; Função que ordena uma produção
(define-metafunction G
  ord-prd : nt rhs -> prd
  [(ord-prd nt ((trm t ...) ... (nt_1 t_0 ...) (nt_2 t_1 ...) ... (trm_0 t_2 ...) seq ...))
   (ord-prd nt ((trm t ...) ... (trm_0 t_2 ...) (nt_1 t_0 ...) (nt_2 t_1 ...) ... seq ...))]

  [(ord-prd nt ((trm t ...) ... (nt_0 t_0 ...) ...))
    (nt (concat-rhs ((trm t ...) ...)  (ord-nt nt () ((nt_0 t_0 ...) ...))))])

; Função que ordena os nt de um rhs
(define-metafunction G
  ord-nt : nt rhs rhs -> rhs
  [(ord-nt nt (seq ...)((nt t ...) seq_1 ...))
   (ord-nt nt ((nt t ...) seq ...)(seq_1 ...))]
  
  [(ord-nt (name n0 nt_!_0) (seq ...)(((name n1 nt_!_0) t ...) seq_1 ...))
   (ord-nt n0 (seq ... (n1 t ...))(seq_1 ...))]
  
  [(ord-nt nt rhs ()) rhs])
   
; Função que unifica duas gramaticas
(define-metafunction G
  concat-prds : prds prds -> prds
  [(concat-prds (prd_1 ... ) (prd_2 ... )) 
   (prd_1 ... prd_2 ... )])

; Função que unifica dois rhs
(define-metafunction G
  concat-rhs : rhs rhs -> rhs
  [(concat-rhs (seq ...) (seq_0 ...))
   (seq ... seq_0 ...)])

; Função que ordena a gramática
(define (ord-rhs prds)
  (define nts (add-flag (remove-duplicates (map car prds))))
  (list nts
    (map
      (lambda (p)
        (define rhs (car (cdr p)))
        (define head (car p))

        (let ((trm (filter (lambda (x) (or (number? (car x)) (equal? (car x) '()))) rhs))
              (nt (filter (lambda (x) (and (not (number? (car x))) (not (equal? (car x) '())) (not (equal? (car x) head)))) rhs))
              (recursion (filter (lambda (x) (and (not (number? (car x))) (not (equal? (car x) '())) (equal? (car x) head))) rhs)))
          (cons (car p) (list (append trm recursion nt)))))
    prds)))

; Função que adiciona um flag 0 em cada não trm ou seja (A 0)
(define (add-flag nts)
  (map (lambda (x) (list x 0)) nts))
  
; Função que remove elementos repetidos de uma lista
(define (remove-duplicates lst)
  (cond
    [(empty? lst) empty]
    [(member (first lst) (rest lst)) (remove-duplicates (rest lst))]
    [else (cons (first lst) (remove-duplicates (rest lst)))]))

; Função que unifica produções repetidas
(define (unify-prds prds)
  (define (helper prds result)
    (if (null? prds)
        (reverse result)
        (let* ((current (car prds))
               (key (car current))
               (rest (cdr current))
               (existing (assoc key result)))
          (if existing
              (helper (cdr prds)
                      (cons (cons key (list (concat-list (car (cdr existing)) (car rest)))) (filter (lambda (x) (not (equal? key (car x)))) result)))
              (helper (cdr prds)
                      (cons current result))))))
  (helper prds '()))

; Função que concatena duas listas
(define (concat-list lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (concat-list (cdr lst1) lst2))))
