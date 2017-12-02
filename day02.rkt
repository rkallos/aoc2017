#lang racket

(module+ test
  (require rackunit rackunit/text-ui))

(define (string->list-of-integer s)
  (map (λ (c) (string->number c)) (string-split s)))

(define (part1 txt)
  (for/sum ([l (in-lines (open-input-string txt))])
    (define r (string->list-of-integer l))
    (- (apply max r) (apply min r))))

(define (part2 txt)
  (for/sum ([l (in-lines (open-input-string txt))])
    (for/first ([p (in-combinations (string->list-of-integer l) 2)]
                #:when (or (zero? (remainder (first p) (second p)))
                           (zero? (remainder (second p) (first p)))))
      (match-define (list i j) p)
      (/ (max i j) (min i j)))))

(module+ test
  (define-test-suite tests
    (check-eq? (part1 "5 1 9 5\n7 5 3\n2 4 6 8") 18)
    (check-eq? (part2 "5 9 2 8\n9 4 7 3\n3 8 6 5") 9))
  (run-tests tests))
