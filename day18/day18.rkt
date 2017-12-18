#lang racket

(require data/queue)

(struct prog (ip is rs mbox p2 waiting? sends) #:mutable #:transparent)

(define (make-prog is)
  (prog 0 is (make-hash) (make-queue) #f #f 0))

(define (parse-file input)
  (apply vector (map parse-line (file->lines input))))

(define (parse-line line)
  (define (parse-reg r)
    (if (string->number r)
        (string->number r)
        (string-ref r 0)))
  (match (string-split line)
    [(list inst x y)
     (list inst (parse-reg x) (parse-reg y))]
    [(list inst x)
     (list inst (parse-reg x))]))

(define (get p r)
  (if (char? r)
      (hash-ref (prog-rs p) r 0)
      r))

(define is-part1?
  (make-parameter #t))

(define (eval-instruction! p)
  (match-define (prog ip is rs mb _ _ _) p)
  (match (vector-ref is ip)
    [(list "snd" x)
     (if (is-part1?)
         (hash-update! rs 'sounds (λ (l) (cons (get p x) l)) '())
         (send-msg p (get p x)))
     (set-prog-ip! p (add1 ip))]
    [(list "set" x y)
     (hash-set! rs x (get p y))
     (set-prog-ip! p (add1 ip))]
    [(list "add" x y)
     (hash-set! rs x (+ (get p x) (get p y)))
     (set-prog-ip! p (add1 ip))]
    [(list "mul" x y)
     (hash-set! rs x (* (get p x) (get p y)))
     (set-prog-ip! p (add1 ip))]
    [(list "mod" x y)
     (hash-set! rs x (remainder (get p x) (get p y)))
     (set-prog-ip! p (add1 ip))]
    [(list "rcv" x)
     (if (is-part1?)
         (unless (zero? x)
           (hash-set! rs x (first (get p 'sounds)))
           (set-prog-ip! p (add1 ip)))
         (begin
           (recv-msg p x)
           (unless (prog-waiting? p)
             (set-prog-ip! p (add1 ip)))))]
    [(list "jgz" x y)
     (set-prog-ip! p
      (+ ip (if (> (get p x) 0) (get p y) 1)))]))

(define (send-msg p x)
  (define p2 (prog-p2 p))
  (define mbox2 (prog-mbox p2))
  (enqueue! mbox2 x)
  (set-prog-sends! p (add1 (prog-sends p)))
  (set-prog-waiting?! p2 #f))

(define (recv-msg p x)
  (define mbox (prog-mbox p))
  (if (queue-empty? mbox)
      (begin
        (set-prog-waiting?! p #t))
      (begin
        (hash-set! (prog-rs p) x (dequeue! mbox)))))

(define (part1 input)
  (define is (parse-file input))
  (define p (make-prog is))
  (let loop ([ip 0])
    (define i (vector-ref (prog-is p) (prog-ip p)))
    (cond
     [(and (equal? (first i) "rcv")
           (not (equal? 0 (second i)))
           (hash-has-key? (prog-rs p) 'sounds))
      (first (hash-ref (prog-rs p) 'sounds))]
     [else
      (loop (eval-instruction! p))])))

(define (prog-done? p)
  (or (< (prog-ip p) 0)
      (>= (prog-ip p) (vector-length (prog-is p)))))

(define (part2 input)
  (define is (parse-file input))
  (define p1 (make-prog is))
  (define p2 (make-prog is))
  (set-prog-p2! p1 p2)
  (set-prog-p2! p2 p1)
  (hash-set! (prog-rs p1) #\p 0)
  (hash-set! (prog-rs p2) #\p 1)
  (parameterize ([is-part1? #f])
    (let loop ()
      (let loop1 ()
        (unless (or (prog-done? p1) (prog-waiting? p1))
          (eval-instruction! p1)
          (loop1)))
      (let loop2 ()
        (unless (or (prog-done? p2) (prog-waiting? p2))
          (eval-instruction! p2)
          (loop2)))
      (if (and (or (prog-done? p1) (prog-waiting? p1))
               (or (prog-done? p2) (prog-waiting? p2)))
          (prog-sends p2)
          (loop)))))
