#lang racket

(define(star1 initial input)
  (foldl (lambda (elem v)
           (+ v elem)) initial (input-eater-1 input)))

(define (input-eater-1 text)
  (let ([nums (string-split text)])
    (map string->number nums)))

(define hash (make-hash))

(define(star2 initial input)
  (star2
   (foldl (lambda (elem v)
           (when (hash-ref hash v #f)
             (print " ")(print v)(print " " )(print " did it! ")
             (exit))
           (hash-set! hash v #t)
           (+ elem v))
         initial (input-eater-1 input))
   input))

(define (star3 input)
  (let ([codes (input-eater input string->list)] [twos 0] [threes 0])
    (map (lambda (x)
           (when(checkn x 2)(set! twos (+ twos 1)))) codes)
    (map (lambda (x)
           (when(checkn x 3)(set! threes (+ threes 1)))) codes)
    (* twos threes)))

(define (checkn text n)
  ; checks if a list has n (no more or less) matching elements
  (let ([hash (make-hash)] [ret #f])
    (map (lambda (x) (hash-set! hash x (+ 1 (hash-ref hash x 0)))) text)
    (hash-map hash (lambda (x y) (when (equal? n y) (set! ret #t))) #f)
  ret))

(define (input-eater text fun)
    (let ([words (string-split text)])
    (map fun words)))

(define (star4 input)
  (let ([codes (input-eater input string->list)] [first (list)] [second (list)])
    (map (lambda (x)
           (map (lambda (y)
                  (when (check-match x y) (set! first x)(set! second y)))
                codes)
           ) codes) (display first) (display second) (list first second)))

(define debug #t)
(define (prtln x)
  (when debug
  (display "\n")
  (display x)))

(define (check-match x y)
  (let ([goal (- (length x) 1)] [tally 0][i 0])
    (when (equal? (length x) (length y))
      (map (lambda (a)
           (when (equal? a (list-ref y i)) (set! tally (+ 1 tally)))
           (set! i (+ 1 i))) x)
      )(equal? tally goal)))

(define (star5 input)
  (let ([hash (make-hash)][codes (string-split input "\n")])
    {map (lambda (code)
           (let* ([tokens (string-split code)][offsets (substring (list-ref tokens 2) 0 (- (string-length (list-ref tokens 2)) 1)) ][dimensions (list-ref tokens 3)])
             (let ([xoffset (string->number (list-ref (string-split offsets ",") 0))]
                   [yoffset (string->number (list-ref (string-split offsets ",") 1))]
                   [xsize (string->number (list-ref (string-split dimensions "x") 0))]
                   [ysize (string->number (list-ref (string-split dimensions "x") 1))])
               (mark-coords-hash (add-offset-to-coords xoffset yoffset (list-rect xsize ysize)) hash)
               ))) codes}
    (count (lambda (x) (values x))(hash-map hash (lambda (key value) (> value 1))))))
(define (list-rect width height)
  (let ([tot (* width height)])
    (build-list tot (lambda (x)
                                 (cons
                                  (modulo x width)
                                  (quotient x width))))))
(define (add-offset-to-coords x y coords)
  (map (lambda (coord) (cons (+ x (car coord)) (+ y (cdr coord)))) coords))
(define (mark-coords-hash coords hash)
  (map (lambda (coord) (hash-set! hash coord (+ 1 (hash-ref hash coord 0)))) coords))
    


;use filter on hash?
        
(star5 "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2") ; outputs 4
  
