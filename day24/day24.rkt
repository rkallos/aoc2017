#lang racket

(define (file->edges f)
  (for/list ([line (file->lines f)])
    (match line
      [(regexp #rx"([0-9]+)/([0-9]+)" (list _ a b))
       (list (string->number a)
             (string->number b))])))

(define (outgoing-edges vert edges)
  (filter
   (λ (e)
     (match e
       [(list-no-order v o)
        #:when (equal? v vert)
        o]
       [_ #f]))
   edges))

(define (dfs edges [stack (set (list 0 (set)))] [seen (set)])
  (cond
   [(set-empty? stack) seen]
   [else
    (match-define (list cur-vert path) (set-first stack))
    (cond
     [(set-member? seen path)
      (dfs edges (set-rest stack) seen)]
     [else
      (define out-paths
        (map
         (λ (oe)
           (match oe
             [(list-no-order a b)
              #:when (equal? a cur-vert)
              (list b (set-add path oe))]))
         (outgoing-edges cur-vert edges)))
      (dfs edges
           (set-union stack (list->set out-paths))
           (set-add seen path))])]))

(define (bridge-strength bridge)
  (apply + (flatten (set->list bridge))))

(define (part1 f)
  (define bridges (dfs (file->edges f)))
  (apply max (map bridge-strength (set->list bridges))))

(define (part2 f)
  (define bridges (dfs (file->edges f)))
  (define longest-bridge-length
    (apply max (map set-count (set->list bridges))))
  (define longest-bridges
    (filter (λ (b) (equal? (set-count b) longest-bridge-length))
            (set->list bridges)))
  (apply max (map bridge-strength (set->list longest-bridges))))
