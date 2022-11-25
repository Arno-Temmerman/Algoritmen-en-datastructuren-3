#lang r7rs

; Maak bestand file-system-label.rkt

(import (scheme base)
        (scheme write)
        (prefix (a-d disk config) disk:)
        ;(prefix (a-d disk file-system) fs:) ; Dit is de onaangepaste file-system library
        (prefix (a-d disk _5-file-system-label) fs:) ; Importeer de aangepaste file-system library
        )

(define d (disk:new "mijnDisk")) ; Een disk is NIET hetzelfde als een file system
(display d) (newline)

; Maak een file system aan op de reeds aangemaakte disk:
;(fs:format! d)                 ; Oude stijl, cf. (a-d disk file-system)
(fs:format! d "mijnFileSystem") ; Nieuwe stijl, cf. (Hoofdstuk14 file-system-label)

; Lees en toon het label:
(display (fs:label (fs:read-meta-block d))) ; toont "mijnFileSy" omdat maximum lengte 10 is

(disk:unmount d)