#lang racket

(module+ test
  (require rackunit))

(define (vector-update! vec idx f)
  (vector-set! vec idx (f (vector-ref vec idx))))

(define (redistribute l)
  (define vec (apply vector l))
  (match-define (list bank-amt bank-idx)
    (for/fold ([m '(0 -1)])
              ([idx (in-range (vector-length vec))])
      (define bank (vector-ref vec idx))
      (if (> bank (first m)) (list bank idx) m)))
  (vector-set! vec bank-idx 0)
  (for ([i (build-list
            bank-amt (λ (b) (remainder (+ 1 b bank-idx) (vector-length vec))))])
    (vector-update! vec i add1))
  (vector->list vec))

(define (solve txt)
  (define l (map (λ (s) (string->number s)) (string-split txt)))
  (let loop ([l l] [h (hash)] [steps 0])
    (cond
      [(hash-has-key? h l)
       (list steps (- steps (hash-ref h l)))]
      [else
       (loop (redistribute l) (hash-set h l steps) (add1 steps))])))

(module+ test
  (check-equal? (solve "0 2 7 0") (list 5 4)))
