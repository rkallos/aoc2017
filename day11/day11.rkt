#lang racket

(require pict)

(module+ test
  (require rackunit))

(define (string->steps input)
  (string-split input #px"\n|,"))

(define return (make-parameter first))

(define (solve steps [x 0] [y 0] [z 0] [farthest 0])
  (cond
    [(empty? steps)
     (list (steps-to x y z) farthest)]
    [else
     (define-values (dx dy dz)
       (case (first steps)
         [("n")  (values  0  1 -1)]
         [("ne") (values  1  0 -1)]
         [("nw") (values -1  1  0)]
         [("s")  (values  0 -1  1)]
         [("se") (values  1 -1  0)]
         [("sw") (values -1  0  1)]))
     (define-values (x2 y2 z2)
       (values (+ x dx) (+ y dy) (+ z dz)))
     (solve (rest steps) x2 y2 z2
            (max farthest (steps-to x2 y2 z2)))]))

(define (steps-to x y z)
  (/ (+ (abs x) (abs y) (abs z)) 2))

(define (part1 input)
  (define steps (string->steps input))
  ((return) (solve steps)))

(module+ test
  (check-equal? (part1 "ne,ne,ne") 3)
  (check-equal? (part1 "ne,ne,sw,sw") 0)
  (check-equal? (part1 "ne,ne,s,s") 2)
  (check-equal? (part1 "se,sw,se,sw,sw") 3))

(define (part2 input)
  (parameterize ([return second])
    (part1 input)))

(module+ test
  (check-equal? (part2 "ne,ne,ne") 3)
  (check-equal? (part2 "ne,ne,sw,sw") 2)
  (check-equal? (part2 "ne,ne,s,s") 2)
  (check-equal? (part2 "se,sw,se,sw,sw") 3))
