#lang racket

(provide (all-defined-out))

(require redex     
    "cfggen/main.rkt"
    "cfggen/cyk.rkt"
    "cfggen/grammar-transforms2.rkt"
    "cfggen/util/structs.rkt"
    "struct.rkt")

(define (numterm->symbol t)
  (string->symbol (string (integer->char (+ t 97)))))

(define (grammar-to-cnf input)
  (define counter 1)
  (define terminals '())

  (define (generate-new-nt nt)
    (let ((new-nt (string->symbol (string-append "AUX" (number->string counter)))))
      (set! counter (+ counter 1))
      new-nt))

  (define (num->symbol n)
    (let ((sym (string->symbol (string-append "T" (number->string n)))))
      (unless (assoc sym terminals)
        (set! terminals (cons (cons sym (numterm->symbol n)) terminals)))
      sym))

  (define (format-prd prd)
    (match prd
      [(Production (NT nt) alt)
       (let ((formatted-rhs (coalese-alt (rewrite-rhs alt))))
         (flatten (map (lambda (rhs) (process-rhs nt rhs)) formatted-rhs)))]))

  (define (rewrite-rhs rhs)
    (match rhs 
      [(Alt e d) (Alt (rewrite-rhs e) (rewrite-rhs d))]
      [(Seq e d) (append (rewrite-rhs e) (rewrite-rhs d))]
      [(NT nt)   (list nt)] 
      [(T t) (list (num->symbol t))]
      [#t (list)]))

  (define (coalese-alt rhs)
    (match rhs 
      [(Alt e d) (append (coalese-alt e) (coalese-alt d))]
      [x (list x)]))

  (define (process-rhs nt rhs)
    (if (<= (length rhs) 2)
        (list (cons nt (list rhs))) 
        (let ((new-nt (generate-new-nt nt)))
          (append (list (list nt (list (car rhs) new-nt)))
                  (process-rhs new-nt (cdr rhs))))))

  (define (flatten list-of-lists)
    (apply append list-of-lists))

  (define formatted-input (flatten (map format-prd (reform-grammar input))))
  (define terminal-productions (map (lambda (t) (list (car t) (list (cdr t))))
                                    terminals))
  (append formatted-input terminal-productions)
  )

 

(define (grammar-to-cyk g [start (NT-String (Production-nt (car g)))])
        ;(post-process-grm (fix-cyk (grammar-to-cnf (remove-unreachables (remove-ε g)))))
          (post-process-grm (fix-cyk (grammar-to-cnf (remove-ε g))) start)
    )
 
(define (is-terminal? x)
       (and (symbol? x)
            (string>=? (symbol->string x) "a")
            (string<=? (symbol->string x) "z")))

(define (rule-body g r)
   (cond
     [(null? g) (error (string-append "Undefined name: " (~v r)))]
     [(equal? (car (car g)) r) (cdr (car g))]
     [else (rule-body (cdr g) r)]))

(define (rule-all-bodys g r)
   (cond
     [(null? g) '()]
     [(equal? (car (car g)) r) (if (list? (cdr (car g)))
                                (set-union (cdr (car g)) (rule-all-bodys (cdr g) r))
                                (rule-all-bodys (cdr g) r))]
     [else (rule-all-bodys (cdr g) r)]))

(define (romve-uesless g)
    (define (is-usefull? r)
      (match r
          [(list n n) #f]
          [_ #t]))
   (filter is-usefull? g))

(define (replace-unit g)
    (map (lambda (r)
            (cond
              [(and (list? r)
                    (= 2 (length r))
                       ) (cons (car r) (rule-body g (cadr r)) )]
              [else r])
           ) g))

(define (non-eps? r)
   (match r
       [(list n '()) #f]
       [_ #t]))

(define (fix-cyk g)
        (map (lambda (xs)
               (if (is-terminal? (car (car (cdr xs))))
                   (cons (car xs) (car (car (cdr xs))))
                   (cons (car xs) (car (cdr xs)))))
             (filter non-eps? g)))

(define (candidates xs q)
        (filter (lambda (z) (not (member z q))) xs))

(define (reachs g [start (car (car g))] )
     (define (rchs g q r)       
          (cond
            [(null? q) (remove-duplicates r)]
            [else (rchs g
                        (append (cdr q) (candidates (rule-all-bodys g (car q)) (set-union q r)))
                        (cons (car q) r)) ]
       ))
       (rchs g (list start) '()))

(define (remove-unreach g [start (car (car g))] )
     (let ([rs (reachs g start)])
          (filter (lambda (r) (member (car r) rs)) g)))

(define (post-process-grm g [start (car (car g))])
   ;(remove-unreach (replace-unit (romve-uesless g)))
    (remove-unreach (replace-unit (romve-uesless g)) start)
  )

(define (accept-on-cyk g word [start (NT-String (Production-nt (car g)))])
        (let ([sword (map numterm->symbol word)]
              [nullNts (nullable-NTs g) ]
              [gcyk (grammar-to-cyk g start)])
              (if (and (null? word) (member  start nullNts) )
                  #t
                  (car (cyk sword gcyk (make-vector 1 start)))
              ))
   )

#;(define teste '((S ((1 2) (A B 2) (B B 2))) 
               (A1 (() (3 2 A1) (1 3 A1))) 
               (A ((1 1 A1))) 
               (B1 (() (1 1 B1))) 
               (B ((1 3 B1)))))


;(define testeF (format-input teste))
;(define testeI '(1 2))
;(displayln (grammar-to-cnf testeF))

