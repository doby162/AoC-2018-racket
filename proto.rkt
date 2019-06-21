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
;make a bitlist an exact number of bytes
(define (pad-bitlist bits)
  (let ([add (- 8 (modulo (length bits) 8))])
    (when (not (= 8 add)) (set! bits (append (make-list add 0) bits))) bits))
;turn decimal numbers into a string of bits for twiddling with
(define (int->bits dec [top true])
  (let([bin-buffer (list)])
    (when (> dec 0)    
      (set! bin-buffer (append bin-buffer (list (modulo dec 2))))
      (set! bin-buffer (append bin-buffer (int->bits (floor (/ dec 2)) false))))
    (when top (set! bin-buffer (pad-bitlist (reverse bin-buffer))))
    bin-buffer))
;define a record
(struct record (type timestamp user-id dollar-amount))
(define records (list))
;and create a record
(define (read-record file pointer)
  (let ([type (reader 1)])
  (set! records (append records (list
                                 (record type (reader 4) (reader 8)
                                         (when (or (= type 0) (= type 1)) (bitlist->float (int->bits (reader 8))))))))
    (inspect (- (length records) 1))))
;display a record
(define (inspect n)
  (let ([element (list-ref records n)])
    (fprintf (current-output-port) "type:~a~ntimestamp:~a~nuser-id:~a~ndollar-amount:~a~n"
             (record-type element)(record-timestamp element)(record-user-id element)(record-dollar-amount element))))
;similar to reading bytes from a file, but works for arbitrary lists of bits
(define (bitlist->int bitlist)
  (let ([value 0] [bits bitlist])
    (map (lambda (bit)
           (set! value (* 2 value))
           (set! value (+ bit value))) bits) value))
;trim both edges off of a list.
(define (list-section list beg distance)
  (list-tail (reverse (list-tail (reverse list)  (- (length list) distance beg))) beg))
;simulate a hardware float64
(define exponent-bias 1023)
(define (bitlist->float bitlist)
  (let ([sign (list-ref bitlist 0)] [exp (bitlist->int (list-section bitlist 1 11))] [num (list-tail bitlist 12)] )
    (* (expt 2 (- exp exponent-bias)) (string->number (string-append* "1." (map number->string num)) 2))))

;process the header and calibrate the pointer
(define name (subbytes in 0 4))
(define version (bytes->int (subbytes in 4 5)))
(define record-num (bytes->int (subbytes in 5 9)))
(define pointer 9)

;steps
(fprintf (current-output-port) "Name: ~a~nVersion: ~a~nRecords: ~a~n" name version record-num)
(define (read-all [n 0]) (when (< n record-num) (read-record in pointer) (read-all (+ 1 n))))
(read-all)
;(inspect 0) inspect specific records  

