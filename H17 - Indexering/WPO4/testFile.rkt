#lang r7rs

;; BE SURE TO UNCOMMENT either (a-d disk disk) or (a-d disk cached-disk-wbwa) in config.rkt (a-d disk config) 
;; AVOID (a-d disk cached-disk) as the block locking system might shoot you in the foot...

(import (prefix (a-d disk config) disk:)
        (prefix (a-d disk file-system) unix:)
        (prefix (Hoofdstuk17 Oplossingen database+) db:)
        (prefix (Hoofdstuk17 Oplossingen btree+) btree:) 
        (scheme base)
        (scheme write))

;--------- Exercises HFST 17 -----------

;Create a new disk and format it
(define my-disk (disk:new "HardDisk"))
(unix:format! my-disk)

;Create a schema for planets
(define planetenschema '((string 9)
                         (decimal)
                         (decimal)
                         (natural 3)
                         (decimal)
                         (decimal)))

; Name your attributes
(define :planeet-naam:    0)
(define :afstand-tot-zon: 1)
(define :aard-massa:      2)
(define :middellijn:      3)
(define :omlooptijd:      4)
(define :rotatietijd:     5)

;Create a list of planet records
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

; Define a new database
(define zonnestelsel (db:new my-disk "Zonnestelsel"))

; Create a new table in the database
; IMPORTANT: keep lengths of names up to 10 characters! (see file-system.rkt fs:filename-size)
(define planeten-tbl  (db:create-table zonnestelsel "Planeten" planetenschema))

; Insert a list of records in a table
(define (insert-records db table records)
  (cond ((null? records) 'done)
        (else
         (db:insert-into-table! db table (car records))
         (insert-records db table (cdr records)))))

; Fill table in db and print to show
(insert-records zonnestelsel planeten-tbl planeten)
(db:print-table zonnestelsel planeten-tbl)
(newline)

; Create an index on one of the attributes
; IMPORTANT: keep lengths of names up to 10 characters! (see file-system.rkt fs:filename-size)
(define index-name "plt-naam")

(db:create-index! zonnestelsel planeten-tbl index-name :planeet-naam:)

; To print the corresponding B+-tree, we will need to retrieve it from disk first
(define plt-indx (btree:open my-disk index-name))
(btree:print plt-indx)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEST YOUR SOLUTION ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; Uncomment exercise per exercise to test

; Ex. 1
; Indexed attribute
(display "Eerste planeet volgens alfabet: ")
(display (db:select-from/min zonnestelsel planeten-tbl :planeet-naam:)) (newline)
(display "Laatste planeet volgens alfabet: ")
(display (db:select-from/max zonnestelsel planeten-tbl :planeet-naam:)) (newline)
(newline)

; Unindexed attribute
(display "Lichtste planeet volgens aardmassa: ")
(display (db:select-from/min zonnestelsel planeten-tbl :aard-massa:)) (newline)
(display "Zwaarste planeet volgens aardmassa: ")
(display (db:select-from/max zonnestelsel planeten-tbl :aard-massa:)) (newline)
(newline)

; Ex. 2
; Use only on indexed attributes (otherwise errors)
; Ascending
(display "Alle planeten alfabetisch oplopend : ")
(display (db:select-from/all/ordered zonnestelsel planeten-tbl :planeet-naam: #t)) (newline)
(newline)

; Descending
(display "Alle planeten alfabetisch aflopend : ")
(display (db:select-from/all/ordered zonnestelsel planeten-tbl :planeet-naam: #f)) (newline) 
(newline)

; Ex. 3a
; Unindexed attribute
(display "Alle planeten met aardmassa vanaf 1 tot en met 14.5 : ")
(display (db:select-from/range/incl zonnestelsel planeten-tbl :aard-massa: 1 14.5))(newline)
(newline)
(display "Alle planeten met aardmassa tot en met 14.5 : ")
(display (db:select-from/range/incl zonnestelsel planeten-tbl :aard-massa: '() 14.5))(newline)
(newline)
(display "Alle planeten met aardmassa vanaf 1 : ")
(display (db:select-from/range/incl zonnestelsel planeten-tbl :aard-massa: 1 '()))(newline)
(newline)

; Indexed attribute
(display "Alle planeten vanaf Mercurius tot en met Uranus : ")
(display (db:select-from/range/incl zonnestelsel planeten-tbl :planeet-naam: "Mercurius" "Uranus")) (newline)
(newline)
(display "Alle planeten tot en met Uranus : ")
(display (db:select-from/range/incl zonnestelsel planeten-tbl :planeet-naam: '() "Uranus")) (newline)
(newline)
(display "Alle planeten vanaf Mercurius : ")
(display (db:select-from/range/incl zonnestelsel planeten-tbl :planeet-naam: "Mercurius" '())) (newline)
(newline)
(display "Alle planeten tot en met Marc : ") ; typo intended
(display (db:select-from/range/incl zonnestelsel planeten-tbl :planeet-naam: '() "Marc")) (newline)
(newline)
(display "Alle planeten vanaf Marc : ") ; typo intended
(display (db:select-from/range/incl zonnestelsel planeten-tbl :planeet-naam: "Marc" '() )) (newline)
(newline)

; Ex. 3b
; Unindexed attribute
(display "Alle planeten met aardmassa tussen 1 en 17.5 : ")
(display (db:select-from/range zonnestelsel planeten-tbl :aard-massa: 1 17.5 #f #f))(newline)
(newline)
(display "Alle planeten met aardmassa vanaf 1 aardmassa tot 14.5 : ")
(display (db:select-from/range zonnestelsel planeten-tbl :aard-massa: 1 14.5 #t #f))(newline)
(newline)
(display "Alle planeten met aardmassa tot en met 100 : ")
(display (db:select-from/range zonnestelsel planeten-tbl :aard-massa: '() 100 #f #t))(newline)
(newline)

; Indexed attribute
(display "Alle planeten tussen Mercurius en Uranus : ")
(display (db:select-from/range zonnestelsel planeten-tbl :planeet-naam: "Mercurius" "Uranus" #f #f)) (newline)
(newline)
(display "Alle planete voor Uranus : ")
(display (db:select-from/range zonnestelsel planeten-tbl :planeet-naam: '() "Uranus" #t #f)) (newline)
(newline)
(display "Alle planeten na Mercurius : ")
(display (db:select-from/range zonnestelsel planeten-tbl :planeet-naam: "Mercurius" '() #f #t)) (newline)
(newline)
(display "Alle planeten voor Marc : ") ; typo intended
(display (db:select-from/range zonnestelsel planeten-tbl :planeet-naam: '() "Marc" #t #f)) (newline)
(newline)
(display "Alle planeten na Marc : ") ; typo intended
(display (db:select-from/range zonnestelsel planeten-tbl :planeet-naam: "Marc" '() #f #t)) (newline)
(newline)

(disk:unmount my-disk)