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
                                 (record (reader 1) (reader 4) (reader 8) (int->bits (reader 8)))))))
;display a record
(define (inspect n)
  (let ([element (list-ref records n)])
    (fprintf (current-output-port) "type:~a~ntimestamp:~a~nuser-id:~a~ndollar-amount:~a~n"
             (record-type element)(record-timestamp element)(record-user-id element)(record-dollar-amount element))))
    
;process the header
(define name (subbytes in 0 4))
(define version (bytes->int (subbytes in 4 5)))
(define record-num (bytes->int (subbytes in 5 9)))
(define pointer 9)



;bytes-ref
;steps
(read-record in pointer)
(inspect 0)

(define (bitlist->int bitlist)
  (let ([value 0] [bits (reverse bitlist)])
    (map (lambda (bit)
           (set! value (* 2 value))
           (set! value (+ bit value))) bits) value))


;(bitlist->int (record-dollar-amount (list-ref records 0)))
;10011100001111