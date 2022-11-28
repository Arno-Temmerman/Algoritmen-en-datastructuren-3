#lang r7rs

(import (scheme base)
        (scheme write)
        (prefix (a-d disk config) disk:)
        (prefix (a-d disk file-system) fs:)
        (prefix (a-d file sequential sequential-file) seq:)
        (prefix (a-d file sequential input-file) in:)
        (prefix (a-d file sequential output-file) out:))

(define d (disk:new "mijnDisk")) ; Maak een disk
(display d) (newline)

(display "Formatting...")
(fs:format! d) ; Maak een file system
(display " done") (newline)

(define f1 (out:new d "file1")) ; Maak een eerste file
; Schrijf de volgende strings weg; we hebben hiervoor normaliter 2 content blocks nodig:
(out:write! f1 "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam in suscipit sem. Sed amet. ")
(out:write! f1 "Suspendisse justo massa, imperdiet eu interdum mattis, placerat sed mauris. Donec sit sed.")
(out:close-write! f1)

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

(newline)
(stat d "file1") ; File telt normaliter 1 header block en 2 content blocks

; Debugging-functie die de inhoud van een bestand toont:
(define (cat disk file-name)
  (define file (in:open-read! disk file-name))
  (let loop ()
    (if (in:has-more? file)
        (begin (display (in:read file))
               (loop)))))

(cat d "file1") (newline)

; Oefening 6, deel (a): High level copy

;;; TODO

(newline)
;(define f2 (high-level-copy d "file1" "file2"))
;(stat d "file2")
;(cat d "file2") (newline)

; Oefening 6, deel (b): Low level copy

;;; TODO

(newline)
;(define f3 (low-level-copy d "file2" "file3"))
;(stat d "file3")
;(cat d "file3") (newline)
