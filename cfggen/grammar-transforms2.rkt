#lang racket

(require "./util/constants.rkt"
         "./util/predicates.rkt"
         "./util/reducers.rkt"
         "./util/structs.rkt"
         "./delta.rkt")

(provide nullable-NTs
         remove-ε
         distribute-grammar
         reform-grammar
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

(define (simplify-ε rhs)
  (match rhs
    [(Seq #t d) (simplify-ε d)]
    [(Seq e #t) (simplify-ε e)]
    [(Alt #t d) (simplify-ε d)]
    [(Alt e #t) (simplify-ε e)]
    [x x]
  ))
  

(define (remove-ε grammar [start (Production-nt (car grammar)) ])
  (define nullable-set (reverse (nullable-NTs grammar)))
  #;(delete-ε-prod (reform-grammar (_remove-ε grammar nullable-set)) (list start))
  (distribute-grammar (_remove-ε grammar nullable-set))
  )

(define (rem-ε-seq rhs ns)
      (match rhs
        [(Seq e d) (Seq (rem-ε-seq e ns) (rem-ε-seq d ns))]
        [(Alt e d) (Alt (rem-ε-seq e ns) (rem-ε-seq d ns))]
        [(NT t)  (cond [(member t ns) (Alt rhs #t) ]
                       [else           rhs])]
        [x x]
        )
  )

(define (_remove-ε grammar nullable-set)
      (define (r-eps p)
          (Production (Production-nt p) (rem-ε-seq (Production-rhs p) nullable-set))
       )
      (map r-eps grammar)
    )

(define (replace-productions-to-NT productions prod-name rhs) ; TODO: repensar o nome
  (match productions
    [(cons (Production (NT l) r) xs) (cons (Production (NT l) (replace-rhs-to-NT r prod-name rhs)) (replace-productions-to-NT xs prod-name rhs))]
    ['() '()]
  ))

(define (replace-rhs-to-NT production prod-name rhs)
  (match production
    [(Seq l r) (simplify-ε (seq (replace-rhs-to-NT l prod-name rhs) (replace-rhs-to-NT r prod-name rhs)))]
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

; redistributes sequences over alternatives trougth the whole grammar
; Also reformats ands and alts to tehir left associative form.
(define (distribute-grammar g)
      (map distribute-alts g))

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
            [(cons (Alt _ _ ) (Alt _ _))  (Alt (build-seq e) (build-seq d))]
            [(cons _ (Alt _ _))   (ins-seq-l e d)]
            [(cons (Alt _ _) _)   (ins-seq-r e d)]
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
    [#t (Seq #t ie)]
    ))

(define (ins-seq-l ie rhs )
  (match rhs
    [(Alt e d) (Alt (ins-seq-l ie e) (ins-seq-l ie d))] 
    [(Seq e d) (Seq ie (Seq e d))] 
    [(NT v) (Seq ie (NT v) )]
    [(T t)  (Seq ie (T t))]
    [#t (Seq ie #t)]
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

(define (get-nt-body g name)
   (cond
     [(null? g) #f]
     [(equal? (NT-String (Production-nt (car g))) name) (Production-rhs (car g))]
     [else (get-nt-body (cdr g) name)])
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
       (let ([reaches (remove-duplicates  (nts-from-rhs (get-nt-body g (car q))) )])
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
(define (prod->string p)
    (string-append (symbol->string (NT-String (Production-nt p)))
                   " -> "
                   (rhs->string (Production-rhs p) 15)
   )
 )

(define (parens b s)  (if b (string-append "(" s ")") s))
; Precedence :
; Bases Lit, NT, Eps. 0
; Seq 5
; Alt 10
(define (rhs->string rhs prec) ;; TODO: esse print aqui pode ser usado pra depurar o restante do projeto
  (cond ((rhs-empty? rhs) "ε")
        ((rhs-invalid? rhs) "∅")
        (else (match rhs
                [(Seq e d) (parens (< prec 5) (string-append (rhs->string e 5)
                                                             " "
                                                             (rhs->string d 4)))]
                [(Alt e d) (parens (< prec 10) (string-append (rhs->string e 10)
                                                             " | "
                                                             (rhs->string d 9)))]
                [(T x) (if (number? x)
                           (number->string x)
                           (symbol->string x))]
                [(NT x) (symbol->string x)]))))

(define (grammar->string list)
  (if (empty? list)
      "\n"
      (string-append (prod->string (car list)) "\n" (grammar->string (cdr list)))))

(define (print-grammar g)
    (display (grammar->string g))
  )