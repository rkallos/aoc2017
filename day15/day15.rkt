#lang racket

(require racket/generator)

(module+ test
  (require rackunit))

(define fac-a 16807)
(define fac-b 48271)

(define (solve gen-a gen-b rounds)
  (for/fold ([matches 0])
            ([r rounds])
    (if (equal? (bitwise-bit-field (gen-a) 1 16)
                (bitwise-bit-field (gen-b) 1 16))
        (add1 matches)
        matches)))

(define (mk-gen x x-fac)
  (generator ()
    (let loop ([x x])
      (define x2 (remainder (* x x-fac) 2147483647))
      (yield x2)
      (loop x2))))

(define (mk-rem-gen x x-fac rem)
  (generator ()
    (let loop ([x x])
      (define x2 (remainder (* x x-fac) 2147483647))
      (cond
        [(zero? (remainder x2 rem))
         (yield x2)
         (loop x2)]
        [else (loop x2)]))))
          

(define (part1 a0 b0 [rounds 40000000])
  (solve (mk-gen a0 fac-a) (mk-gen b0 fac-b) rounds))

(define (part2 a0 b0 [rounds 5000000])
  (solve (mk-rem-gen a0 fac-a 4) (mk-rem-gen b0 fac-b 8) rounds))

(module+ test
  (check-equal? (part1 65 8921 5) 1)
  (check-equal? (part2 65 8921 1056) 1))
