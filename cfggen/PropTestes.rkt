#lang racket
(require rackcheck
         "main.rkt"
         "../gerador.rkt"
         "../struct.rkt")

(define g2
  (list
 (Production
  (NT 'S)
  (Alt
   (Seq (NT 'S) (Seq (NT 'B) (Seq (NT 'S) (Seq (NT 'A) (T 3)))))
   (Alt (Seq (NT 'S) (Seq (NT 'B) (Seq (NT 'A) (Seq (NT 'B) (T 3)))))
        (Seq (T 2) (T 3)))))
 (Production (NT 'A) (Alt (Seq (NT 'A) (Seq (T 3) (Seq (T 2) (Seq (NT 'B) (T 3)))))
                          (Seq (T 3) (Seq (T 3) (T 2)))))
 (Production (NT 'B) (Alt (Seq (NT 'B) (Seq (T 1) (T 1)))
                          (Seq (T 2) (Seq (T 3) (T 1)))))))

(define safe-gen (property safe-gen ([w (gen:word-from-grammar g2)])
                                    (if (or (eq? w #f) (eq? w ∅))
                                       #t
                                       (in-grammar? g2 w )
                                       )
     ))

(check-property safe-gen)