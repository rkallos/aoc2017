#lang racket

(module+ test
  (require rackunit))

(define (scanner-posn range time)
  (if (zero? range)
      0
      (add1 (if (even? (quotient time (sub1 range)))
                (remainder time (sub1 range))
                (- range 1 (remainder time (sub1 range)))))))

(define (parse-lines lines [pairs '()])
  (define-values (pairs max-depth)
    (for/fold ([ps '()] [md -1])
            ([line lines])
    (define pair (map string->number (regexp-match* #px"[0-9]+" line)))
    (values (cons pair ps) (max md (car pair)))))
  (define v (make-vector (add1 max-depth) 0))
  (for ([pair pairs])
    (vector-set! v (first pair) (second pair)))
  v)

(define (severity v [idx 0] [t 0] [sev 0] [caught #f])
  (cond
    [(= idx (vector-length v)) (list sev caught)]
    [(= 1 (scanner-posn (vector-ref v idx) t))
     (severity v (add1 idx) (add1 t)
               (+ sev (* idx (vector-ref v idx))) idx)]
    [else
     (severity v (add1 idx) (add1 t) sev caught)]))

(define (part1 input)
  (first (severity (parse-lines (file->lines input)))))

(module+ test
  (check-equal? (part1 "test") 24))

(define (caught? v t)
  (second (severity v 0 t)))

(define (delay v [t 1])
  (define c (caught? v t))
  (cond
    [(not c) t]
    [else
     (delay v (add1 t))]))

(define (part2 input)
  (delay (parse-lines (file->lines input))))

(module+ test
  (check-equal? (part2 "test") 10))