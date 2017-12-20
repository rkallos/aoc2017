#lang racket

(struct mol (x y z dx dy dz ddx ddy ddz) #:mutable #:transparent)

(define (file->mols file)
  (map parse-line (file->lines file)))

(define (parse-line line)
  (define numbers
    (map string->number
         (regexp-match* #rx"-?[0-9]+" line)))
  (match numbers
    [(list x y z dx dy dz ddx ddy ddz)
     (mol x y z dx dy dz ddx ddy ddz)]))

(define (manhattan coords)
  (apply + (map abs coords)))

(define (part1 file)
  (define mols (file->mols file))
  (for/fold ([mini '(0 +inf.0)]) ([m mols] [i (in-range (length mols))])
    (define md (manhattan (list (mol-ddx m) (mol-ddy m) (mol-ddz m))))
    (if (< md (second mini))
        (list i md)
        mini)))

(define (move! m)
  (match-define (mol x y z dx dy dz ddx ddy ddz) m)
  (set-mol-dx! m (+ dx ddx))
  (set-mol-dy! m (+ dy ddy))
  (set-mol-dz! m (+ dz ddz))
  (set-mol-x! m (+ x (mol-dx m)))
  (set-mol-y! m (+ y (mol-dy m)))
  (set-mol-z! m (+ z (mol-dz m))))

(define (remove-collisions mols)
  (define (mol-pos m)
    (list (mol-x m) (mol-y m) (mol-z m)))
  (define points
    (foldl (λ (e a) (hash-update a (mol-pos e) add1 (thunk 0)))
           (hash) mols))
  (define collisions
    (list->set
     (filter (λ (e) (> (hash-ref points e) 1)) (hash-keys points))))
  (filter (λ (e) (not (set-member? collisions (mol-pos e)))) mols))

(define (part2 file)
  (let loop ([mols (file->mols file)]
             [t 0])
    (cond
     [(empty? mols) 0]
     [(> t 1000) (length mols)]
     [else
      (for-each move! mols)
      (loop (remove-collisions mols) (add1 t))])))
