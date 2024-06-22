#lang racket

(require "./util/constants.rkt"
         "./util/predicates.rkt"
         "./util/reducers.rkt"
         "./util/structs.rkt"
         "./delta.rkt")

(provide nullable-NTs
         remove-ε
         distribute-grammar
         delete-ε-prod
         reachable
         remove-unreachables
         print-grammar
  )

; S -> B D
; B -> C b | ε
; C -> c
; D -> d | aD | ε


(define g1 (list
 (Production (NT 'S) (Seq (NT 'B) (NT 'D)) )
 (Production (NT 'B) (Alt (Seq (NT 'C) (T 'b)) ε))
 (Production (NT 'C) (Alt (T 'c) (NT 'D)))
 (Production (NT 'D) (Alt (T 'd) (Alt (Seq (T 'a) (NT 'D)) ε)))
 ))

;; 0) Remover regras ε
(define (has-ε? rhs nullable-set)
  (match rhs
    [(Alt l r) (or (has-ε? l nullable-set) (has-ε? r nullable-set))]
    [(Seq l r) (and (has-ε? l nullable-set) (has-ε? r nullable-set))]
    [(T x) #f]
    [(NT x) (not (eq? (member x nullable-set) #f))]
    [ε #t]
    [_ #f] ;; TODO: aqui vai ser sempre #t, refatorar as constantes.
  ))

(define (nullable-prods grammar [initial-nullable-set '()]) ; TODO: repensar o nome
  (match grammar
    [(cons (Production (NT l) r) xs) (if (has-ε? r initial-nullable-set)
                                         (cons l (nullable-prods xs initial-nullable-set))
                                         (nullable-prods xs initial-nullable-set))]
    ['() '()]
  ))

(define (nullable-NTs grammar [initial-nullable-set '()])
  (define nullable-set (nullable-prods grammar initial-nullable-set))
  (if (> (length nullable-set) (length initial-nullable-set))
      (nullable-NTs grammar nullable-set)
      nullable-set))

(define (symplify-ε rhs)
  (match rhs
    [(Seq #t (Alt x1 x2)) (alt x1 x2)]
    [(Seq x1 (Alt #t x2)) (alt x1 (seq x1 x2))]
    [(Seq x1 (Alt x2 #t)) (alt (seq x1 x2) x1)]

    [(Seq (Alt #t x1) x2) (alt x2 (seq x1 x2))]
    [(Seq (Alt x1 #t) x2) (alt (seq x1 x2) x2)]
    [(Seq (Alt x1 x2) #t) (alt x1 x2)]

    [(Seq (Alt x1 x2) x3) (cond
                            ((rhs-delta 'NOOP x2 #:ignore-NT #t) (alt (seq x1 x3) (symplify-ε (seq x2 x3))))
                            ((rhs-delta 'NOOP x1 #:ignore-NT #t) (alt (symplify-ε (seq x1 x3)) (seq x2 x3)))
                            (else (seq (alt x1 x2) x3))
                            )]
    [(Seq x1 (Alt x2 x3)) (cond
                            ((rhs-delta 'NOOP x3 #:ignore-NT #t) (alt (seq x1 x2) (symplify-ε (seq x1 x3))))
                            ((rhs-delta 'NOOP x2 #:ignore-NT #t) (alt (symplify-ε (seq x1 x2)) (seq x1 x3)))
                            (else (seq x1 (alt x2 x3)))
                            )]
    [x x]
  ))
  

(define (remove-ε grammar [start (Production-nt (car grammar)) ])
  (define nullable-set (reverse (nullable-NTs grammar)))
  (delete-ε-prod (reform-grammar (_remove-ε grammar nullable-set)) (list start))
  )

(define (_remove-ε grammar nullable-set)
  (if (null? nullable-set)
      grammar
      (let ([evaluated-NT (car nullable-set)])
           (let ([rhs (find-named-rhs evaluated-NT grammar)])
                (if (empty? (cdr nullable-set))
                    (replace-productions-to-NT grammar evaluated-NT rhs)
                    (_remove-ε (replace-productions-to-NT grammar evaluated-NT rhs) (cdr nullable-set))
    )))))

(define (replace-productions-to-NT productions prod-name rhs) ; TODO: repensar o nome
  (match productions
    [(cons (Production (NT l) r) xs) (cons (Production (NT l) (replace-rhs-to-NT r prod-name rhs)) (replace-productions-to-NT xs prod-name rhs))]
    ['() '()]
  ))

(define (replace-rhs-to-NT production prod-name rhs)
  (match production
    [(Seq l r) (symplify-ε (seq (replace-rhs-to-NT l prod-name rhs) (replace-rhs-to-NT r prod-name rhs)))]
    [(Alt l r) (alt (replace-rhs-to-NT l prod-name rhs) (replace-rhs-to-NT r prod-name rhs))]
    [(NT x) (if (eq? x prod-name) rhs (NT x))]
    [x x]
  ))

(define (find-named-rhs prod-name grammar)
  (define production (car grammar))
  (if (is-named-prod? prod-name production)
      (get-rhs production)
      (find-named-rhs prod-name (cdr grammar))))

(define (is-named-prod? prod-name production)
  (match production
    [(Production (NT x) _) (eq? prod-name x)]
    [_ #f]))

(define (get-rhs production)
  (match production
    [(Production (NT _) rhs) rhs]
    [_ #f]))

; Makes a trasformations distributting sequences onver alternatives
; Example (a | b)c => ac | bc
; Alos normalizes sequences and alterntavies in theis expected fromat
; Exs.: a (b c) => (a b) c         ;(Seq a (Seq b c)) => (Seq (Seq a b) c)
;       a | (b | c) => (a | b) | c ;(Alt a (Alt b c)) => (Alt (Alt a b) c)
(define (distribute-alts production)
  (match production
    [(Production lhs rhs) (Production lhs (reform-rhs (distribute-alts-rhs rhs)))]
    [_ (error ( (~v production) " not an production"))]))

(define (distribute-alts-rhs rhs)
  (match rhs
    [(Seq (Alt ee ed) d) (let ([ee1 (distribute-alts-rhs ee)]
                               [ed1 (distribute-alts-rhs ed)]
                               [d1  (distribute-alts-rhs d)])
                              (Alt (ins-seq-r ee1 d1) (ins-seq-r ed1 d1)))] 
    [(Seq e (Alt de dd)) (let ([e1   (distribute-alts-rhs e)]
                               [de1  (distribute-alts-rhs de)]
                               [dd1  (distribute-alts-rhs dd)])
                              (Alt (ins-seq-l e1 de1 ) (ins-seq-l e1 dd1)))]
    [(Seq e d) (let ([e1 (distribute-alts-rhs e)]
                     [d1 (distribute-alts-rhs d)])
                     (build-seq e1 d1))] 
    [(Alt e d) (Alt (distribute-alts-rhs e) (distribute-alts-rhs d) )]
    [x x]
    ))
(define (build-seq e d)
     (match (cons e d)
            [(cons (Seq _ _) (Seq _ _))     (Seq e d)]
            [(cons (Seq _ _) (Alt de dd))   (ins-seq-l e d)]
            [(cons (Alt ee ed) (Seq _ _))   (ins-seq-r e d)]
            [(cons (Alt _ _ ) (Alt _ _))    (Alt e d)]
            [_ (Seq e d)]
       )
  )

; Axuliary funtcion to the redustribution procees
; Ex. rhs = (a | b), ie = c
; results = (ac | bc)
(define (ins-seq-r rhs ie)
  (match rhs
    [(Alt e d) (Alt (ins-seq-r e ie) (ins-seq-r d ie))] 
    [(Seq e d) (Seq e (Seq d ie))] 
    [(NT v) (Seq (NT v) ie)]
    [(T t)  (Seq (T t) ie)]
    ))

(define (ins-seq-l ie rhs )
  (match rhs
    [(Alt e d) (Alt (ins-seq-l ie e) (ins-seq-l ie d))] 
    [(Seq e d) (Seq ie (Seq e d))] 
    [(NT v) (Seq ie (NT v) )]
    [(T t)  (Seq ie (T t))]
    ))


; Reformats ands and alts to be in the left associative form.
(define (reform-rhs rhs)
    (match rhs
      [(Seq (Seq ee ed) (Seq de dd)) (Seq (Seq (Seq (reform-rhs ee) (reform-rhs ed))
                                               (reform-rhs de))
                                          (reform-rhs dd))]
      [(Seq (Seq ee ed) d) (Seq (Seq (reform-rhs ee) (reform-rhs ed))
                                (reform-rhs d))]
      [(Seq e (Seq de dd)) (Seq (Seq (reform-rhs e) (reform-rhs de))
                                (reform-rhs dd))]
      [(Alt (Alt ee ed) (Alt de dd)) (Alt (Alt (Alt (reform-rhs ee) (reform-rhs ed))
                                               (reform-rhs de))
                                          (reform-rhs dd))]
      [(Alt (Alt ee ed) d) (Alt (Alt (reform-rhs ee) (reform-rhs ed))
                                (reform-rhs d))]
      [(Alt e (Alt de dd)) (Alt (Alt (reform-rhs e) (reform-rhs de))
                                (reform-rhs dd))]
      [x x]
      )
  )
; redistributes sequences over alternatives trougth the whole grammar
; Also reformats ands and alts to tehir left associative form.
(define (distribute-grammar g)
      (map distribute-alts g))

(define (reform-grammar g)
     (define (rfrm p)
         (Production (Production-nt p) (reform-rhs (Production-rhs p))))
     (map rfrm (distribute-grammar g)
  ))

(define (is-empty-prod? p)
   (match p
     [(Production nt #t) #f]
     [(Production nt _) #t])
  )

; To be used after the ε-removal process. Romove any ε production, except 
; from the symbols given the imuneList.
(define (delete-ε-prod g imuneList)
  (define (helper p)
     (cond
       [(member  (Production-nt p)  imuneList)  p]
       [else (Production (Production-nt p) (simplify-ε-rhs (Production-rhs p) ))]))
   (filter (lambda (x) (not (equal? (Production-rhs x) #t))) (map helper g))
  )

(define  (simplify-ε-rhs rhs)
  (match rhs
    [(Alt #t #t) #t]
    [(Alt #t d) (simplify-ε-rhs d)]
    [(Alt e #t) (simplify-ε-rhs e)]
    [(Alt e d) (Alt (simplify-ε-rhs e) (simplify-ε-rhs d))]
    [(Seq #t #t) #t]
    [(Seq #t d) (simplify-ε-rhs d)]
    [(Seq e #t) (simplify-ε-rhs e)]
    [(Seq e d) (Seq (simplify-ε-rhs e) (simplify-ε-rhs d))]
    [x x]
   ))

(define (grammar-prod g name)
   (cond
     [(null? g) #f]
     [(equal? (NT-String (Production-nt (car g))) name) (Production-rhs (car g))]
     [else (grammar-prod (cdr g) name)])
 )

(define (nts-from-rhs rhs)
     (match rhs
        [(Alt e d) (set-union (nts-from-rhs e) (nts-from-rhs d))]
        [(Seq e d) (set-union (nts-from-rhs e) (nts-from-rhs d))]
        [(NT n) (list n)]
        [_ (list )])
)

(define (reachable g (start (NT-String (Production-nt (car g)))))
     (reach-q g (list start) (list))       
)

(define (reach-q g q r)'D
   (if (null? q)
       (set-union q r)
       (let ([reaches (remove-duplicates  (nts-from-rhs (grammar-prod g (car q))) )])
            (reach-q g (append (cdr q)
                               (filter (lambda (x) (not (equal? (car q) x)) ) reaches))
                       (set-union q r)))
       )
   )

(define (remove-unreachables g (start (NT-String (Production-nt (car g)))) )
    (let ([xs (reachable g)])
         (filter (lambda (x)
                         (member (NT-String (Production-nt x)) xs))
                 g)
  ))
  
;; 1) Converter pra CNF.
;; 2) Converter pra interface do cyk
;; 3) Rodar o teste.

;; Test
#;(define g2 '((S . (S A))
             (S . b)
             (A . a)))

#;(define starts1 #(S))
#;(can-parse? '(b a a a) g2 starts1)

;;;;;; PRINTING

(define (rhs->string rhs) ;; TODO: esse print aqui pode ser usado pra depurar o restante do projeto
  (cond ((rhs-empty? rhs) "ε")
        ((rhs-invalid? rhs) "∅")
        (else (match rhs
                [(Production l r) (string-append (rhs->string l) " -> " (rhs->string r))]
                [(Seq x1 (Alt x2 x3)) (string-append (rhs->string x1) "(" (rhs->string x2) " | " (rhs->string x3) ")")]
                [(Seq (Alt x1 x2) x3) (string-append "(" (rhs->string x1) " | " (rhs->string x2) ")" (rhs->string x3) )]
                [(Seq l r) (string-append (rhs->string l) " " (rhs->string r))]
                [(Alt l r) (string-append (rhs->string l) " | " (rhs->string r))]
                [(T x) (if (number? x) (number->string x) (symbol->string x))]
                [(NT x) (symbol->string x)]))))

(define (grammar->string list)
  (if (empty? list)
      "\n"
      (string-append (rhs->string (car list)) "\n" (grammar->string (cdr list)))))

(define (print-grammar g)
    (display (grammar->string g))
  )