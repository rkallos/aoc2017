#lang racket

(module+ test
  (require rackunit))

(struct state (status outer-score sum))

(define (enter-garbage s)
  (struct-copy state s [status 'garbage]))

(define (exit-garbage s)
  (struct-copy state s [status 'ok]))

(define (enter-group s)
  (match s
    [(struct state ('garbage _ _)) s]
    [(struct state ('ok os total))
     (state 'ok (add1 os) total)]))

(define (exit-group s)
  (match s
    [(struct state ('garbage _ _)) s]
    [(struct state ('ok os total))
     (state 'ok (sub1 os) (+ total os))]))

(define (parse1 chars state)
  (match chars
    ['() state]
    [(cons #\! r) (parse1 (rest r) state)]
    [(cons #\> r) (parse1 r (exit-garbage state))]
    [(cons #\< r) (parse1 r (enter-garbage state))]
    [(cons #\{ r) (parse1 r (enter-group state))]
    [(cons #\} r) (parse1 r (exit-group state))]
    [(cons _ r)   (parse1 r state)]))

(define (part1 txt)
  (state-sum (parse1 (string->list txt) (state 'ok 0 0))))

(module+ test
  (check-equal? (part1 "{}") 1)
  (check-equal? (part1 "{{{}}}") 6)
  (check-equal? (part1 "{{},{}}") 5)
  (check-equal? (part1 "{{{},{},{{}}}}") 16)
  (check-equal? (part1 "{<a>,<a>,<a>,<a>}") 1)
  (check-equal? (part1 "{{<ab>},{<ab>},{<ab>},{<ab>}}") 9)
  (check-equal? (part1 "{{<!!>},{<!!>},{<!!>},{<!!>}}") 9)
  (check-equal? (part1 "{{<a!>},{<a!>},{<a!>},{<ab>}}") 3))

(define (consume-char s)
  (match s
    [(struct state ('garbage _ total))
     (struct-copy state s [sum (add1 total)])]
    [_ s]))

(define (parse2 chars state)
  (match chars
    ['() state]
    [(cons #\! r) (parse2 (rest r) state)]
    [(cons #\> r) (parse2 r (exit-garbage state))]
    [(cons #\< r)
     (if (equal? (state-status state) 'garbage)
         (parse2 r (consume-char state))
         (parse2 r (enter-garbage state)))]
    [(cons _ r)   (parse2 r (consume-char state))]))

(define (part2 txt)
  (state-sum (parse2 (string->list txt) (state 'ok 0 0))))

(module+ test
  (check-equal? (part2 "<>") 0)
  (check-equal? (part2 "<random characters>") 17)
  (check-equal? (part2 "<<<<>") 3)
  (check-equal? (part2 "<{!>}>") 2)
  (check-equal? (part2 "<!!>") 0)
  (check-equal? (part2 "<!!!>>") 0)
  (check-equal? (part2 "<{o\"i!a,<{i<a>") 10))