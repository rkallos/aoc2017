#lang racket

(module+ test
  (require rackunit))

(define (solve in chg)
  (define states
    (apply vector
           (map (λ (l) (string->number l))
                (file->lines in))))
  (define exit (vector-length states))
  (let loop ([idx 0] [steps 0])
    (cond
      [(>= idx exit) steps]
      [else
       (define cur-val (vector-ref states idx))
       (define new-idx (+ idx cur-val))
       (vector-set! states idx (chg cur-val))
       (loop new-idx (add1 steps))])))

(define (part1 in)
  (solve in add1))

(module+ test
  (check-eq? (part1 "test") 5))

(define (part2 in)
  (solve in (λ (v) (if (>= v 3) (sub1 v) (add1 v)))))

(module+ test
  (check-eq? (part2 "test") 10))
