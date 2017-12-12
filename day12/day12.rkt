#lang racket

(module+ test
  (require rackunit))

(struct uf (parent rank) #:mutable #:transparent)

(define (new-uf a) (uf a 0))

(define (find g x-key)
  (define x (hash-ref g x-key))
  (if (not (equal? x-key (uf-parent x)))
      (find g (uf-parent x))
      x-key))

(define (union! g x-key y-key)
  (define x-root (hash-ref g (find g x-key)))
  (define y-root (hash-ref g (find g y-key)))
  (cond
    [(equal? x-root y-root) g]
    [(< (uf-rank x-root) (uf-rank y-root))
     (set-uf-parent! x-root y-key)]
    [(> (uf-rank x-root) (uf-rank y-root))
     (set-uf-parent! y-root x-key)]
    [else
     (set-uf-parent! y-root x-key)
     (set-uf-rank! x-root (add1 (uf-rank x-root)))]))

(define (solve lines [g (make-hash)])
  (for ([line lines])
    (match-define (list f rest ...)
      (map string->number (regexp-match* #rx"[0-9]+" line)))
    (when (not (hash-has-key? g f))
      (hash-set! g f (new-uf f)))
    (for ([r rest])
      (when (not (hash-has-key? g r))
        (hash-set! g r (new-uf r)))
      (union! g f r)))
  g)

(define (part1 file)
  (define g (solve (file->lines file)))
  (count (λ (v) (equal? (find g v) (find g 0))) (hash-keys g)))

(module+ test
  (check-equal? (part1 "test") 6))

(define (part2 file)
  (define g (solve (file->lines file)))
  (set-count
   (for/set ([k (hash-keys g)])
     (find g k))))

(module+ test
  (check-equal? (part2 "test") 2))
