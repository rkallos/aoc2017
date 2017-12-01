#lang racket

(module+ test
  (require rackunit))

(define (text->digit-vec txt)
  (apply vector-immutable
         (map (λ (c) (- (char->integer c) (char->integer #\0)))
              (string->list txt))))

(define (next-digit v len idx)
  (remainder (add1 idx) len))

(define (halfway-away v len idx)
  (remainder (+ idx (/ len 2)) len))

(define (solve v next-idx-fun)
  (define len (vector-length v))
  (let loop ([idx 0] [sum 0])
    (cond
      [(eq? idx len) sum]
      [(eq? (vector-ref v idx) (vector-ref v (next-idx-fun v len idx)))
       (loop (add1 idx) (+ sum (vector-ref v idx)))]
      [else
       (loop (add1 idx) sum)])))

(define (part1 txt)
  (solve (text->digit-vec txt) next-digit))

(define (part2 txt)
  (solve (text->digit-vec txt) halfway-away))

(module+ test
  (check-eq? (part1 "1122") 3)
  (check-eq? (part1 "1111") 4)
  (check-eq? (part1 "1234") 0)
  (check-eq? (part1 "91212129") 9))

(module+ test
  (check-eq? (part2 "1212") 6)
  (check-eq? (part2 "1221") 0)
  (check-eq? (part2 "123425") 4)
  (check-eq? (part2 "123123") 12)
  (check-eq? (part2 "12131415") 4))
