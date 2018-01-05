#lang racket

(define (file->grid f)
  (define lines (file->lines f))
  (define n (length lines))
  (define n/2 (floor (/ n 2)))
  (define h (make-hash))
  (cons n/2 (read-grid (string->list (file->string f)) n/2 h)))

(define (read-grid cs n/2 h)
  (let loop ([cs cs]
             [x (- n/2)]
             [y (- n/2)])
    (match cs
      ['() h]
      [(cons c cs)
       (match c
         [#\# 
          (hash-set! h (cons x y) 'infected)
          (loop cs (add1 x) y)]
         [#\newline
          (loop cs (- n/2) (add1 y))]
         [_
          (loop cs (add1 x) y)])])))

(define (turn-left dir)
      (match dir
        ['up 'left]
        ['left 'down]
        ['down 'right]
        ['right 'up]))

(define (turn-right dir)
  (match dir
    ['up 'right]
    ['right 'down]
    ['down 'left]
    ['left 'up]))

(define (move-virus x y dir)
  (match dir
    ['up    (cons x (sub1 y))]
    ['down  (cons x (add1 y))]
    ['left  (cons (sub1 x) y)]
    ['right (cons (add1 x) y)]))

(define (count-infections-1 is grid)
  (let loop ([i 0]
             [infections 0]
             [pt (cons 0 0)]
             [dir 'up])
    (define (cur)   (hash-ref grid pt 'clean))
    (define (left)  (turn-left dir))
    (define (right) (turn-right dir))
    (define (move dir)
      (match-define (cons x y) pt)
      (move-virus x y dir))
    (cond
     [(equal? i is) infections]
     [(equal? (cur) 'infected)
      (hash-remove! grid pt)
      (loop (add1 i) infections (move (right)) (right))]
     [else
      (hash-set! grid pt 'infected)
      (loop (add1 i) (add1 infections) (move (left)) (left))])))

(define (part1 f)
  (match-define (cons n/2 grid) (file->grid f))
  (count-infections-1 10000 grid))

(define (turn-back dir)
  (match dir
    ['up 'down]
    ['down 'up]
    ['right 'left]
    ['left 'right]))

(define (count-infections-2 is grid)
  (let loop ([i 0]
             [infections 0]
             [pt (cons 0 0)]
             [dir 'up])
    (define (current-node) (hash-ref grid pt 'clean))
    (define (left)  (turn-left dir))
    (define (right) (turn-right dir))
    (define (back)  (turn-back dir))
    (define (move dir)
      (match-define (cons x y) pt)
      (move-virus x y dir))
    (if (equal? i is)
        infections
        (match (current-node)
          ['clean
           (hash-set! grid pt 'weakened)
           (loop (add1 i) infections (move (left)) (left))]
          ['weakened
           (hash-set! grid pt 'infected)
           (loop (add1 i) (add1 infections) (move dir) dir)]
          ['infected
           (hash-set! grid pt 'flagged)
           (loop (add1 i) infections (move (right)) (right))]
          ['flagged
           (hash-remove! grid pt)
           (loop (add1 i) infections (move (back)) (back))]))))

(define (part2 f)
  (match-define (cons n/2 grid) (file->grid f))
  (count-infections-2 10000000 grid))
