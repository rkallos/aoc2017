#lang racket

(require math/number-theory)

(define (file->prog f)
  (for/vector ([line (file->lines f)])
    (match line
      [(regexp #rx"(.*) (.*) (.*)" (list _ i a b))
       (vector (string->symbol i)
               (string->operand a)
               (string->operand b))])))

(define (string->operand a)
  (if (string->number a)
      (string->number a)
      (first (string->list a))))

(define (mul-count prog [ip 0] [regs (make-hash)] [mc 0])
  (define (get r)
    (if (char? r)
        (hash-ref regs r 0)
        r))
  (define (set r v)
    (hash-set! regs r (get v)))
  (cond
   [(>= ip (vector-length prog)) mc]
   [else
    (match (vector-ref prog ip)
      [(vector 'set x y)
       (set x y)
       (mul-count prog (add1 ip) regs mc)]
      [(vector 'sub x y)
       (set x (- (get x) (get y)))
       (mul-count prog (add1 ip) regs mc)]
      [(vector 'mul x y)
       (set x (* (get x) (get y)))
       (mul-count prog (add1 ip) regs (add1 mc))]
      [(vector 'jnz x y)
       (define new-ip
         (if (zero? (get x))
             (add1 ip)
             (+ ip y)))
       (mul-count prog new-ip regs mc)])]))

(define (part1 f)
  (mul-count (file->prog f)))

(define (part2-2)
  (define h 0)
  (for ([x (in-range 109900 126901 17)]
        #:when (not (prime? x)))
    (set! h (add1 h)))
  h)
