#lang racket

(define (file->maze f)
  (define m (make-hash))
  (let loop ([chars (string->list (file->string f))]
             [y 0]
             [x 0])
    (cond
     [(empty? chars) m]
     [else
      (match (first chars)
        [#\newline
         (loop (rest chars) (add1 y) 0)]
        [#\space
         (loop (rest chars) y (add1 x))]
        [(or #\- #\| #\+)
         (hash-set! m (list x y) (first chars))
         (loop (rest chars) y (add1 x))]
        [(? char-alphabetic? a)
         (hash-set! m (list x y) a)
         (loop (rest chars) y (add1 x))])])))

(define (find-start maze)
  (let loop ([keys (hash-keys maze)])
    (match (first keys)
      [(list x 0) x]
      [else (loop (rest keys))])))

(define (steer maze x y dx dy)
  (define neighbors
    (filter (λ (key) (hash-ref maze key #f))
            (list (list (sub1 x) y)
                  (list (add1 x) y)
                  (list x (sub1 y))
                  (list x (add1 y)))))
  (define prev (list (- x dx) (- y dy)))
  (define out
    (if (equal? prev (first neighbors))
        (second neighbors)
        (first neighbors)))
  (list (- (first out) x)
        (- (second out) y)))

(define (traverse maze)
  (let loop ([x (find-start maze)]
             [y 0] [dx 0] [dy 1]
             [ls '()]
             [steps 0])
    (match (hash-ref maze (list x y) #f)
      [#f
       (list (list->string (reverse ls))
             steps)]
      [#\+
       (match-define (list dx2 dy2)
                     (steer maze x y dx dy))
       (loop (+ x dx2) (+ y dy2) dx2 dy2 ls (add1 steps))]

      [(? char-alphabetic? c)
       (loop (+ x dx) (+ y dy) dx dy (cons c ls) (add1 steps))]
      [(? char? c)
       (loop (+ x dx) (+ y dy) dx dy ls (add1 steps))])))

(define (solve f)
  (define maze (file->maze f))
  (traverse maze))
