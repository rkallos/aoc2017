#lang racket

(define (file->instructions file)
  (define strings (string-split (file->string file) ","))
  (map parse-string strings))

(define (parse-string string)
  (match string
    [(regexp #rx"s([0-9]+)" (list _ n))
     (list 's (string->number n))]
    [(regexp #rx"x([0-9]+)/([0-9]+)" (list _ a b))
     (list 'x (string->number a) (string->number b))]
    [(regexp #rx"p([a-p])/([a-p])" (list _ a b))
     (list 'p (string-ref a 0) (string-ref b 0))]))

(define (do inst dancers)
  (match inst
    [(list 's n)   (swap dancers n)]
    [(list 'x a b) (exchange dancers a b)]
    [(list 'p a b) (partner dancers a b)]))

(define (swap dancers n)
  (define-values (l r)
    (vector-split-at-right dancers n))
  (vector-append r l))

(define (exchange dancers a b)
  (define va (vector-ref dancers a))
  (define vb (vector-ref dancers b))
  (vector-set*! dancers a vb b va)
  dancers)

(define (partner dancers va vb)
  (define a (vector-member va dancers))
  (define b (vector-member vb dancers))
  (exchange dancers a b))

(define (part1 input [dancers "abcdefghijklmnop"])
  (define is (file->instructions input))
  (solve is dancers))

(define (solve is dancers)
  (list->string
   (vector->list
    (foldl (λ (i ds) (do i ds))
           (apply vector (string->list dancers))
           is))))

(define (find-period is ds [n 0] [h (make-hash)])
  (define ds2 (solve is ds))
  (cond
    [(hash-has-key? h ds2) n]
    [else
     (hash-set! h ds2 n)
     (find-period is ds2 (add1 n) h)]))

(define (part2 input [dancers "abcdefghijklmnop"] #:n [n 1000000000])
  (define is (file->instructions input))
  (define period (find-period is dancers))
  (for/fold ([ds dancers])
            ([i (in-range (remainder n period))])
    (solve is ds)))
