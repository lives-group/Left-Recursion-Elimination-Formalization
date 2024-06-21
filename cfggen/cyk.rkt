#lang racket

(require racket/trace racket/vector)
(provide cyk)

(define : vector-ref)
(define := vector-set!)


; CYK Parser
(define (cyk input grammar starts)
  (cond
    ((list? input)(cyk-sent input grammar starts))
    (else '(ERROR: Unsupported input format. Use lists.))))

(define (cyk-sent s g starts)
  (call/cc
   (λ(K)
     (define terminals '())
     (define non-terminals '())
     (define sent (list->vector s))
     (define N (vector-length sent))
     (define M (length g))
     (define rules (make-hash))
     (let ((j 1))
       (dict-for-each g(λ(k v)(hash-set! rules k j)(set! j (+ j 1)))))
     (define P (make-vector (+ N 1)))
     (for ([i (in-range 1 (+ N 1))])
       (:= P i (make-vector (+ N 1)))
       (for ([j (in-range 1 (+ N 1))])
         (:= (: P i) j (make-vector (+ M 1) #f))))
     (for ([i (in-range 1 (+ N 1))])
       (let ((a (: sent (- i 1))))
         (dict-for-each g(λ(k v)(when(eq? v a)
                                  (begin(:=(:(: P i)1)(hash-ref rules k) #t)
                                        (set! terminals (cons (list k '-> v) terminals))))))))
     (set! terminals (reverse terminals))
     (when(< (length terminals)N)(K '(ERROR: Input contains word not given in grammar)))
     (for ([length (in-range 2 (+ N 1))])
       (for ([start (in-range 1 (+(- N length)2))])
         (for ([len1 (in-range 1 length)])
           (let ((len2 (- length len1)))
             (dict-for-each g(λ(k v)(when(pair? v)
                                      (when(and (:(:(: P start)len1)(hash-ref rules(car v)))
                                                (:(:(: P (+ start len1))len2)(hash-ref rules(cadr v))))
                                        (begin (:=(:(: P start)length)(hash-ref rules k) #t)
                                               (set! non-terminals (cons (list k '-> v) non-terminals)))))))
          ))))
     (when(< (length non-terminals)(- N 1))(K (cons #f (cons non-terminals terminals))))
     (define result #f)
     (for ([i (vector-length starts)])
       (for ([j (in-range 1 (+ N 1))])
         (when(eq?(:(:(: P 1)j)(hash-ref rules(: starts i)))#t)(set! result #t))))
     (cons result (cons non-terminals terminals)))))



