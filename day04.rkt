#lang racket

(module+ test
  (require rackunit))

(define (no-repeated-words? p)
  (define phrase (string-split p))
  (eq? (length phrase)
       (set-count (list->set phrase))))

(define (count-valid-passphrases txt pred)
  (define passphrases (string-split txt "\n"))
  (for/sum ([p passphrases]
            #:when (pred p))
    1))

(define (part1 txt)
  (count-valid-passphrases txt no-repeated-words?))

(module+ test
  (check-eq? (part1 "aa bb cc dd ee\naa bb cc dd aa\naa bb cc dd aaa") 2))

(define (no-anagrams? p)
  (define phrase (string-split p))
  (define words-with-sorted-chars
    (map (λ (word)
           (sort (string->list word) char<?))
         phrase))
  (eq? (length words-with-sorted-chars)
       (set-count (list->set words-with-sorted-chars))))

(define (part2 txt)
  (count-valid-passphrases txt no-anagrams?))

(module+ test
  (check-eq? (part2 "abcde fghij\nabcde xyz ecdab\na ab abc abd abf abj
iiii oiii ooii oooi oooo\noiii ioii iioi iiio") 3))
