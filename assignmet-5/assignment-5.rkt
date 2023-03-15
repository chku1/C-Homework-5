#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; add your funciton implementations below

;; 1) TODO: sequence
(define (sequence low high stride)
  (if (> low high)
      '()
      (cons low (sequence (+ low stride) high stride))))

;; 2) TODO: string-append-map
(define (string-append-map xs suffix)
  (if (null? xs)
      null
      (cons (string-append (car xs) suffix)(string-append-map (cdr xs) suffix))))

;; 3) TODO: string-append-map
(define (sum-square-difference n)
      (- (square-of-sums n)(sum-of-squares n)))

(define (sum-of-squares n [acc 0])
  (if (< n 1)
      acc
      (sum-of-squares (- n 1) (+ acc (* n n)))))

(define (square-of-sums n [acc 0])
  (if (< n 1)
      (* acc acc)
      (square-of-sums (- n 1) (+ acc n))))

  

;; 4) TODO: string-append-map
(define (count-nucleotides dna [a_count 0][c_count 0][g_count 0] [t_count 0])
  (cond [(string? dna)(count-nucleotides (string->list dna) a_count c_count g_count t_count)]
        [(null? dna)(values a_count c_count g_count t_count)]
        [(char=? (car dna) #\A)(count-nucleotides (cdr dna) (+ a_count 1) c_count g_count t_count)]
        [(char=? (car dna) #\C) (count-nucleotides (cdr dna) a_count (+ c_count 1) g_count t_count)]
        [(char=? (car dna) #\G) (count-nucleotides (cdr dna) a_count c_count (+ g_count 1) t_count)]
        [(char=? (car dna) #\T) (count-nucleotides (cdr dna) a_count c_count g_count (+ t_count 1))]
        [(char=? (car dna) #\newline)(count-nucleotides (cdr dna) a_count c_count g_count t_count)]))
        

;; 5) TODO: string-append-map
(define (dna-to-rna dna)
  (list->string(map
   (lambda (ch)
     (cond
       [(char=? ch #\T) #\U]
       [else ch]))
   (string->list dna))))