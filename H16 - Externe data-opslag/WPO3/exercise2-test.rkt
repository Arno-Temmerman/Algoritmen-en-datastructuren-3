#lang r7rs

;; BE SURE TO UNCOMMENT either (a-d disk disk) or (a-d disk cached-disk-wbwa) in config.rkt (a-d disk config) 
;; AVOID (a-d disk cached-disk) as the block locking system might shoot you in the foot...

(import (prefix (a-d disk config) disk:)
        (prefix (a-d disk file-system) unix:)
        (prefix (a-d db table fixed-size-slots _2-table) tbl:) 
        (prefix (a-d db table fixed-size-slots schema) scma:)
        (a-d file constants)
        (scheme base)
        (scheme write)
        (only (racket base) format))

;--------- Exercises HFST 16 -----------

;--------- Test (Ex. 16.2)-----------

; Create a new disk and format it
(define my-disk (disk:new "HardDisk"))
(unix:format! my-disk)

; Create a schema for planets
(define planetenschema '((string 9)
                         (decimal)
                         (decimal)
                         (natural 3)
                         (decimal)
                         (decimal)))

(define :planeet-naam:    0)
(define :afstand-tot-zon: 1)
(define :aard-massa:      2)
(define :middellijn:      3)
(define :omlooptijd:      4)
(define :rotatietijd:     5)

; Create a list of planet records
(define planeten
  (list
   (list "Mercurius" 0.3871   0.053   4840   0.241  +58.79)
   (list "Venus"     0.7233   0.815  12200   0.615 -243.68)
   (list "Aarde"     1.0000   1.000  12756   1.000   +1.00)
   (list "Mars"      1.5237   0.109   6790   1.881   +1.03)
   (list "Jupiter"   5.2028 317.900 142800  11.862   +0.41)
   (list "Saturnus"  9.5388  95.100 119300  29.458   +0.43)
   (list "Uranus"   19.1819  14.500  47100  84.013   -0.45)
   (list "Neptunus" 30.0578  17.500  44800 164.793   +0.63)
   (list "Pluto"    39.2975   1.0     5000 248.43    +0.26)))

;define a new table (internally we create a header file with pointers to full,last and part + a pointer to a schema file)
(define table  (tbl:new my-disk "Planeten" planetenschema)) 

;insert a list of records in a table
(define (insert-records table records)
  (cond ((null? records) 'done)
        (else
         (tbl:insert! table (car records)) 
         (insert-records table (cdr records)))))

;show all records of a table from first 'til last
(define (show-all-records table)
  (define first (tbl:set-current-to-first! table))
  (define (display-iter)
    (define current (tbl:set-current-to-next! table))
    (cond ((eq? current no-current)
           'done)
          (else
           (display (tbl:peek table))
           (newline)
           (display-iter))))
  (if (eq? first no-current)
      'done
      (begin
        (display (tbl:peek table))
        (newline)
        (display-iter))))

;show all records of a table from last 'til first
(define (show-all-records-reversed table)
  (define first (tbl:set-current-to-last! table))
  (define (display-iter)
    (define current (tbl:set-current-to-previous! table))
    (cond ((eq? current no-current)
           'done)
          (else
           (display (tbl:peek table))
           (newline)
           (display-iter))))
  (if (eq? first no-current)
      'done
      (begin
        (display (tbl:peek table))
        (newline)
        (display-iter))))

;execute

(insert-records table planeten)
(show-all-records table)
(display "--------------------\n")
(show-all-records-reversed table)
(disk:unmount my-disk) ;;  don't forget to clean-up after you're done (especially when using cached disks)