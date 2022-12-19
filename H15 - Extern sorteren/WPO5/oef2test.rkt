#lang r7rs

(import (scheme base)
        (scheme write)
        (only (racket base) random) ; random functie
        (prefix (a-d disk config) disk:)
        (prefix (a-d disk file-system) fs:)
        (prefix (a-d file sequential output-file) out:)
        (prefix (a-d file sequential input-file) in:)
        (prefix (a-d file sequential sequential-file) seq:)
        (Hoofdstuk15 Oplossingen multiway-merge-sort-heap-replace-naive)) ; aangepaste sort! functie

; 1. Maak en formatteer een schijf voor de input van het sorteren
(define inputDisk (disk:new "inputDisk"))
(fs:format! inputDisk)

; 2. Maak en formatteer 2*p hulpschijven
(define (makeAuxDisks p)
  (define result (make-vector (* 2 p) 'placeholder))
  (let loop ((i 0))
    (if (< i (* 2 p))
        (let ((auxDisk (disk:new (string-append "auxDisk" (number->string i)))))
          (fs:format! auxDisk)
          (vector-set! result i auxDisk)
          (loop (+ i 1)))))
  result)

(define auxDisks (makeAuxDisks 2))
(newline)

; 3. Maak een bestand met random getallen aan op de inputschijf
(define inputFile (out:new inputDisk "inputFile"))
(define (fillFile count max)
  (let loop ((i 0))
    (if (< i count)
        (begin (out:write! inputFile (random max))
               (loop (+ i 1))))))
(fillFile 100 10000)
(out:close-write! inputFile) ; Vergeet het bestand niet te sluiten na het schrijven!

; Debugging-functie die toont welke blocks een file inneemt:
(define (stat disk file-name)
  (define file (in:open-read! disk file-name))
  (define header (seq:header file))
  (display "File: ") (display (seq:name file)) (newline)
  (display "Blocks: ") (display (disk:position header)) (display " (header)")
  (let loop ((current-bptr (seq:first (seq:header file))))
    (let* ((current-block (disk:read-block disk current-bptr))
           (next-bptr (fs:next-bptr current-block))
           (has-next? (not (fs:null-block? next-bptr))))
      (display ", ") (display current-bptr)
      (if has-next?
          (loop (fs:next-bptr current-block)))))
  (newline))

(stat inputDisk "inputFile")
(newline)

; Debugging-functie die de inhoud van een bestand toont:
(define (cat disk file-name)
  (define file (in:open-read! disk file-name))
  (let loop ()
    (if (in:has-more? file)
        (begin (display (in:read file)) (display " ")
               (loop))))
  (newline))

(cat inputDisk "inputFile")
(newline)

; 4. Sorteer het bestand met multiway-merge-sort
(out:reread! inputFile) ; Open het bestand voor lezen!
(sort! inputFile auxDisks <)

(cat inputDisk "inputFile")
(newline)

(disk:unmount inputDisk)