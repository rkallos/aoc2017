#lang racket

(require file/sha1)

(module+ test
  (require rackunit))

(define (part1 input [len 256])
  (define vec (build-vector len (λ (x) x)))
  (define knots (map string->number (string-split input ",")))
  (solve vec knots)
  (* (vector-ref vec 0) (vector-ref vec 1)))

(define (solve vec knots [idx 0] [skip 0])
  (cond
    [(empty? knots) (void)]
    [else
     (solve vec (cdr knots) (+ (make-twist vec (car knots) idx) skip) (add1 skip))]))

(define (make-twist vec knot idx)
  (define (cycle-idx s)
    (remainder s (vector-length vec)))
  (define twist
    (reverse
     (for/list ([i (in-range knot)])
       (vector-ref vec (cycle-idx (+ idx i))))))
  (for ([i (in-range knot)]
        [e twist])
    (vector-set! vec (cycle-idx (+ idx i)) e))
  (cycle-idx (+ idx knot)))

(module+ test
  (check-equal? (part1 "3,4,1,5" 5) 12))

(define (part2 input)
  (define ascii (append (map char->integer (string->list input))
                        (list 17 31 73 47 23)))
  (define sequence (append* (build-list 64 (λ (x) ascii))))
  (define vec (build-vector 256 (λ (x) x)))
  (solve vec sequence)
  (bytes->hex-string (list->bytes (dense-hash vec))))

(define (dense-hash vec)
  (define lst (vector->list vec))
  (for/list ([i (in-range 0 256 16)])
    (apply bitwise-xor (vector->list (vector-copy vec i (+ i 16))))))

(module+ test
  (check-equal? (part2 "") "a2582a3a0e66e6e86e3812dcb672a272")
  (check-equal? (part2 "AoC 2017") "33efeb34ea91902bb2f59c9920caa6cd")
  (check-equal? (part2 "1,2,3") "3efbe78a8d82f29979031a4aa0b16a9d")
  (check-equal? (part2 "1,2,4") "63960835bcdc130f0b66d7ff4f6a5a8e"))