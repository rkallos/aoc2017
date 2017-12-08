#lang racket

(module+ test
  (require rackunit))

(define (file->instructions file)
  (reverse
   (for/list ([l (file->lines file)])
     (parse l))))

(define (parse line)
  (define pat #rx"([a-z]+) (inc|dec) (-?[0-9]+) if ([a-z]+) (.+) (-?[0-9]+)")
  (cdr (regexp-match pat line)))

(define (eval-instructions is [env (hash)])
  (foldr (λ (i e) (eval-instruction i e)) env is))

(define update (make-parameter
                (λ (h k v)
                  (hash-set h k v))))

(define (eval-instruction i e)
  (define reg (first i))
  (define opr
    (case (second i)
      [("inc") +]
      [("dec") -]))
  (define opd (string->number (third i)))
  (define cond-reg (fourth i))
  (define cmp
    (case (fifth i)
      [(">") >]
      [("<") <]
      [(">=") >=]
      [("<=") <=]
      [("==") =]
      [("!=") (λ (a b) (not (equal? a b)))]))
  (define cmpd (string->number (sixth i)))
  (define reg-val (hash-ref e reg 0))
  (define cond-val (hash-ref e cond-reg 0))
  (if (cmp cond-val cmpd)
      ((update) e reg (opr reg-val opd))
      e))

(define (max-register e)
  (for/fold ([m -inf.0])
            ([v (hash-values e)])
    (if (> v m) v m)))

(define (part1 file)
  (max-register (eval-instructions (file->instructions file))))

(module+ test
  (check-equal? (part1 "./test") 1))

(define (part2 file)
  (parameterize ([update (λ (h k v)
                           (define h2 (hash-set h k v))
                           (if (> v (hash-ref h 'max -inf.0))
                               (hash-set h2 'max v)
                               h2))])
    (hash-ref (eval-instructions (file->instructions file)) 'max)))

(module+ test
  (check-eq? (part2 "./test") 10))