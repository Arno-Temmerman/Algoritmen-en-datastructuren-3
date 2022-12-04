#lang r7rs

; Cf. output-file-extend.rkt, sequential-file-last.rkt, en input-file-last.rkt
; Maak zelf deze aangepaste versies van iedere module en importeer ze hier om te testen.

(import (scheme base)
        (scheme write)
        (prefix (a-d disk config) disk:)
        (prefix (a-d disk file-system) fs:)
        (prefix (a-d file sequential _7-input-file-last) in:) ; Importeer de aangepaste input-file library
        (prefix (a-d file sequential _7-output-file-extend) out:)) ; Importeer de aangepaste output-file library

(define d (disk:new "mijnDisk"))
(display d) (newline)
(fs:format! d)

; Debugging-functie die de inhoud van een bestand toont:
(define (cat disk file-name)
  (define file (in:open-read! disk file-name))
  (let loop ()
    (if (in:has-more? file)
        (begin (display (in:read file))
               (loop)))))

(define f1 (out:new d "file"))
(out:write! f1 "Dit is een test.")
(out:write! f1 " Dit ook.")
(out:close-write! f1)

(cat d "file") (newline)

(define f2 (out:open-write! d "file"))
(out:write! f2 "Dit is een tweede test.") ; Overschrijft de bestaande blocks
(out:close-write! f2)

(cat d "file") (newline)

(define f3 (out:open-extend-write! d "file"))
(out:write! f3 " Dit een derde.") ; Voegt deze data toe achter de bestaande data
(out:close-write! f3)

(cat d "file")

(disk:unmount d)