#lang racket

(require data/gvector)

(define (part1 steps)
  (define gv (make-gvector #:capacity 2017))
  (gvector-add! gv 0)
  (let loop ([from 1] [idx 0])
    (cond
     [(> from 2017) (gvector-ref gv (add1 idx))]
     [else
      (define new-idx (add1 (remainder (+ idx steps) (gvector-count gv))))
      (gvector-insert! gv new-idx from)
      (loop (add1 from) new-idx)])))

(define (part2 steps)
  (let loop ([idx 0] [size 1] [val-after-0 0])
    (cond
     [(= size 50000000) val-after-0]
     [else
      (define new-idx (add1 (remainder (+ idx steps) size)))
      (loop new-idx
            (add1 size)
            (if (= 1 new-idx)
                size
                val-after-0))])))
