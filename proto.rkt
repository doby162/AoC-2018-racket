#lang racket

;read the binary file into a byte string
(define in (port->bytes (open-input-file "txnlog.dat" #:mode'binary)))
;establish a method for interpreting multi byte numbers
(define (bytes->int byte-string)
  (let ([result 0] [bytes (bytes->list byte-string)])
    (map (lambda (byte)
           (set! result
                 (+ (* result (expt 2 8)) byte)))
         bytes) result))
;a shorthand for reading sequential data
(define (reader length)
  (let ([value (bytes->int (subbytes in pointer (+ pointer length)))])
    (set! pointer (+ pointer length))
    value))
;turn decimal numbers into a string of bits for twiddling with
(define (int->bits dec [top true])
  (let([bin-buffer (list)])
    (when (> dec 0)    
      (set! bin-buffer (append bin-buffer (list (modulo dec 2))))
      (set! bin-buffer (append bin-buffer (int->bits (floor (/ dec 2)) false))))
    (when top (set! bin-buffer (reverse bin-buffer)))
    bin-buffer))
;define a record
(struct record (type timestamp user-id dollar-amount))
(define records (list))
;and create a record
(define (read-record file pointer)
  (set! records (append records (list
                                 (record (reader 1) (reader 4) (reader 8) (bitlist->float (int->bits (reader 8))))))))
;display a record
(define (inspect n)
  (let ([element (list-ref records n)])
    (fprintf (current-output-port) "type:~a~ntimestamp:~a~nuser-id:~a~ndollar-amount:~a~n"
             (record-type element)(record-timestamp element)(record-user-id element)(record-dollar-amount element))))
;similar to reading bytes from a file, but works for arbitrary lists of bits
(define (bitlist->int bitlist)
  (let ([value 0] [bits (reverse bitlist)])
    (map (lambda (bit)
           (set! value (* 2 value))
           (set! value (+ bit value))) bits) value))
;trim both edges off of a list.
(define (list-section list beg distance)
  (list-tail (reverse (list-tail (reverse list)  (- (length list) distance beg)  )) beg))
;simulate a hardware float64
(define (bitlist->float bitlist)
  (fprintf (current-output-port) "~n~nbyte1 ~a~nbyte2 ~a~nbyte3 ~a~nbyte4 ~a~nbyte5 ~a~nbyte6 ~a~nbyte7 ~a~nbyte8 ~a~n~n"
           (list-section bitlist 0 8)
           (list-section bitlist 8 8)
           (list-section bitlist 16 8)
           (list-section bitlist 24 8)
           (list-section bitlist 32 8)
           (list-section bitlist 40 8)
           (list-section bitlist 48 8)
           (list-section bitlist 52 8))
  (print (length bitlist))
  (print bitlist)
  (let ([sign (list-ref bitlist 0)] [exp (bitlist->int (list-section bitlist 1 11))] [num (bitlist->int (list-tail bitlist 32))] );604
    (fprintf (current-output-port) "sign: ~a~nexp: ~a~nint: ~a~n" sign exp num)))


;process the header and calibrate the pointer
(define name (subbytes in 0 4))
(define version (bytes->int (subbytes in 4 5)))
(define record-num (bytes->int (subbytes in 5 9)))
(define pointer 9)

;steps
(read-record in pointer)
(inspect 0)

