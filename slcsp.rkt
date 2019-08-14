#lang racket
(require csv-reading)



(define next-row
  (make-csv-reader (open-input-file "slcsp.csv")))