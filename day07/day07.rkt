#lang racket

(module+ test
  (require rackunit))

(struct program (name parent children weight balanced?) #:prefab)

(define (file->programs file)
  (define lines (file->lines file))
  (for/fold ([h (add-programs lines)])
            ([l lines]
             #:when (regexp-match #rx".*->.*" l))
    (add-children h l)))

(define (add-programs lines)
  (for/fold ([h (hash)])
            ([l lines])
    (match-define (list _ name weight)
      (regexp-match #rx"([a-z]+) \\(([0-9]+)\\)" l))
    (hash-set h name (program name '() '() (string->number weight) #t))))

(define (add-children h line)
  (match-define (list _ parent child-str)
    (regexp-match #rx"([a-z]+).*-> (.*)" line))
  (define children (string-split child-str ", "))
  (foldl (λ (ch acc) (add-child acc parent ch)) h children))

(define (add-child h p-key ch-key)
  (define parent (hash-ref h p-key))
  (define cs (program-children parent))
  (define child (hash-ref h ch-key))
  (define h2
    (hash-set h p-key
              (struct-copy program parent (children (cons ch-key cs)))))
  (hash-set h2 ch-key
            (struct-copy program child (parent p-key))))

(define (find-root progs)
  (car (filter (λ (p) (empty? (program-parent p))) progs)))

(define (part1 file)
  (program-name
   (find-root (hash-values (file->programs file)))))

(module+ test
  (check-equal? (part1 "./test") "tknk"))

(define (hash->tree h)
  (make-subtree h (program-name (find-root (hash-values h)))))

(define (make-subtree h p-key)
  (define p (hash-ref h p-key))
  (cond
   [(empty? (program-children p)) p]
   [else
    (struct-copy
     program p
     (children (map (λ (c) (make-subtree h c))
                    (program-children p))))]))

(define (sum-weight tree)
  (define out
    (+ (program-weight tree)
       (apply + (map sum-weight (program-children tree)))))
  (println (string-append "sum-weight: "
                          (program-name tree)
                          " = "
                          (number->string out)
                          " (" (number->string (program-weight tree)) ")"))
  out)

(define (is-balanced? tree)
  (cond
    [(empty? (program-children tree))
     (println (string-append (program-name tree) " is balanced"))
     #t]
    [(zero? (remainder (- (sum-weight tree) (program-weight tree)) 3))
     #t]
    [else
     (andmap is-balanced? (program-children tree))]))

(define (part2 file)
  (define tree (hash->tree (file->programs file)))
  (println (is-balanced? tree))
  0)

(module+ test
  (check-eq? (part2 "./test") 60))
