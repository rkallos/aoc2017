#lang racket

(require data/bit-vector)

(define (file->rules file)
  (apply append
         (map rule-rotations
              (for/list ([line (file->lines file)])
                (line->rule line)))))

(define (line->rule line)
  (match (regexp-match #rx"([./#]+) => ([./#]+)" line)
    [(list _ before after)
     (list (string-split before "/")
           (string-split after  "/"))]))

(define (flip-rule-v r)
  (reverse r))

(define (flip-rule-h r)
  (define (reverse-string s)
    (list->string (reverse (string->list s))))
  (map reverse-string r))

(define (zip2 lst)
  (cond
   [(empty? lst) '()]
   [else
    (cons (list (first lst) (second lst))
          (zip2 (rest (rest lst))))]))

(define (rule-rotations r)
  (match-define (list a b) r)
  (define ts (list (λ (x) x)
                   flip-rule-h
                   flip-rule-v
                   (λ (x) (flip-rule-h (flip-rule-v x)))))
  (set->list
   (for/set ([t ts])
     (list (t a) (t b)))))

(define (2d-set! bv r c v)
  (define r-len (sqrt (bit-vector-length bv)))
  (bit-vector-set! bv (+ (* r r-len) c) v))

(define starting-grid ".#./..#/###")

(define (string->grid s)
  (define size (expt (string-length (first (string-split s "/"))) 2))
  (define bv (make-bit-vector size))
  (let loop ([s (string->list s)] [r 0] [c 0])
    (cond
     [(empty? s) bv]
     [(equal? (first s) #\#)
      (2d-set! bv r c #t)
      (loop (rest s) r (add1 c))]
     [(equal? (first s) #\/)
      (loop (rest s) (add1 r) 0)]
     [else
      (loop (rest s) r (add1 c))])))

(define (part1 file [iterations 5])
  (define rules (file->rules file))
  (let loop ([grid (string->grid starting-grid)] [i 0])
    (cond
     [(= i iterations) (bit-vector-popcount grid)]
     [else
      (loop (transform-grid grid rules) (add1 i))])))

(define (transform-grid grid rules)
  (define grid-len (sqrt (bit-vector-length grid)))
  (define filtered-rules
    (filter (λ (r) (zero? (remainder (length (first r)) grid-len))) rules))
  (define split-grids
    (split-grids grid (if (zero? (remainder? grid-len 2)) 2 3)))
  (println filtered-rules)
  grid)

(define (split-grid grid n)
  )
