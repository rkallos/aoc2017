#lang racket

(module+ test
  (require rackunit))

(define (nth-sq# n)
  (define odd (add1 (* 2 n)))
  (* odd odd))

(define (largest-sq#-< t)
  (let loop ([n 1])
    (cond
      [(< t (nth-sq# n)) (sub1 n)]
      [else (loop (add1 n))])))

(define (new-dy x y dy)
  (cond
    [(and (> (abs x) (abs y)) (>= y 0) (>= x 0)) -1]
    [(and (= (abs x) (abs y)) (>= x 0) (<= y 0))  0]
    [(and (= (abs x) (abs y)) (<= x 0) (<= y 0))  1]
    [(and (= (abs x) (abs y)) (<= x 0) (>= y 0))  0]
    [else dy]))

(define (new-dx x y dx)
  (cond
    [(and (= x (add1 y))      (>= x 0) (>= y 0))  0]
    [(and (= (abs x) (abs y)) (>= y 0) (>= x 0))  1]
    [(and (= (abs x) (abs y)) (>= x 0) (<= y 0)) -1]
    [(and (= (abs x) (abs y)) (<= x 0) (<= y 0))  0]
    [(and (= (abs x) (abs y)) (<= x 0) (>= y 0))  1]
    [else dx]))

(define (part1 in)
  (define lsb (largest-sq#-< in))
  (define start (cons lsb lsb))
  (let loop ([val (nth-sq# lsb)]
             [x lsb] [y lsb]
             [dx 1] [dy 0])
    (if (= val in)
        (+ (abs x) (abs y))
        (loop (add1 val)
              (+ x dx) (+ y dy)
              (new-dx (+ x dx) (+ y dy) dx)
              (new-dy (+ x dx) (+ y dy) dy)))))

(module+ test
  (check-eq? (part1 1) 0)
  (check-eq? (part1 12) 3)
  (check-eq? (part1 23) 2)
  (check-eq? (part1 1024) 31))


(define (neighbors x y)
  (cartesian-product (list (sub1 x) x (add1 x))
                     (list (sub1 y) y (add1 y))))

(define (part2 in)
  (let loop ([grid (make-immutable-hash (list (cons (list 0 0) 1)))]
             [x 1] [y 0] [dx 0] [dy -1])
    (define val-at-pt
      (for/sum ([i (neighbors x y)])
        (if (hash-has-key? grid i)
            (hash-ref grid i)
            0)))
    (if (> val-at-pt in)
        val-at-pt
        (loop (hash-set grid (list x y) val-at-pt)
              (+ x dx) (+ y dy)
              (new-dx (+ x dx) (+ y dy) dx)
              (new-dy (+ x dx) (+ y dy) dy)))))

(module+ test
  (check-eq? (part2 1) 2)
  (check-eq? (part2 5) 10)
  (check-eq? (part2 747) 806))
