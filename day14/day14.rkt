#lang racket

(require data/bit-vector
         file/sha1
         "../day12/day12.rkt"
         (rename-in "../day10.rkt"
                    (part2 knot-hash)))

(module+ test
  (require rackunit))

(define (number->8bits n)
  (define t (format "~B" n))
  (define l (- 8 (string-length t)))
  (string-append (make-string l #\0) t))

(define (string->row-bits str)
  (string->bit-vector
   (apply string-append
          (map number->8bits
               (bytes->list
                (hex-string->bytes
                 (knot-hash str)))))))

(define (string->128-rows str)
  (for/list ([i (in-range 128)])
    (string->row-bits (string-append str "-" (number->string i)))))

(define (part1 input)
  (for/sum ([r (string->128-rows input)])
    (bit-vector-popcount r)))

(define (grid-ref grid n)
  (bit-vector-ref
   (vector-ref grid (first n))
   (second n) #f))

(define (union-with-neighbors! graph grid idx)
  (define (in-bounds? n)
    (and (< -1 (first n) (vector-length grid))
         (< -1 (second n) (bit-vector-length (vector-ref grid (first n))))))
  (match-define (list r c) idx)
  (define neighbors
    (list (list (add1 r) c)
          (list r (add1 c))))
  (for ([n neighbors]
        #:when (and (in-bounds? n)
                    (grid-ref grid n)))
    (union! graph n idx)))

(define (part2 input)
  (define grid (list->vector (string->128-rows input)))
  (define graph (make-hash))
  (for* ([r (in-range 0 128)]
         [c (in-range 0 128)]
         #:when (grid-ref grid (list r c)))
    (unless (hash-has-key? graph (list r c))
      (hash-set! graph (list r c) (new-uf (list r c))))
    (union-with-neighbors! graph grid (list r c)))
  (set-count
   (for/set ([k (hash-keys graph)])
     (find graph k))))

#;
(module+ test
  (check-equal? (part1 "flqrgnkx") 8108)
  (check-equal? (part2 "flqrgnkx") 1242))
