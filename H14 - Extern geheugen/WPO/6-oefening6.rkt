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

(define (high-level-copy disk file-name copy-name)
  (let ((in-file  (in:open-read! disk file-name))
        (out-file (out:new       disk copy-name)))
  
    (let loop ()
      (if (in:has-more? in-file)
          (begin (out:write! out-file (in:read in-file))
                 (loop))))

    (in:close-read!   in-file)
    (out:close-write! out-file)
    out-file))

(newline)
(define f2 (high-level-copy d "file1" "file2"))
(stat d "file2")
(cat d "file2") (newline)

; Oefening 6, deel (b): Low level copy

(define (low-level-copy disk file-name copy-name)
  (let* ((first-copy-block (fs:new-block disk))
         (first-copy-bptr  (disk:position first-copy-block)))
    ; slot aanmaken in dir voor copy-file 
    (fs:mk disk copy-name first-copy-bptr)
  
    (let loop ((file-block (disk:read-block disk (fs:whereis disk file-name)))
               (copy-block first-copy-block))
      (let ((byte-vector (make-bytevector disk:block-size 0)))
        ; overgieten
        (disk:decode-bytes  file-block byte-vector 0 0 disk:block-size)
        (disk:encode-bytes! copy-block byte-vector 0 0 disk:block-size)

        ; Is er nog een volgend block?
        (if (not (fs:null-block? (fs:next-bptr file-block)))
            (let ((next-copy-block (fs:new-block disk)))
              (fs:next-bptr! copy-block (disk:position next-copy-block))
              (disk:write-block! copy-block)
              (loop (disk:read-block disk (fs:next-bptr file-block)) next-copy-block))
            (disk:write-block! copy-block))))))

(newline)
(define f3 (low-level-copy d "file2" "file3"))
(stat d "file3")
(cat d "file3") (newline)
