#lang r7rs

; Vergeet niet de library (a-d disk config) aan te passen (zie verder)!

(import (scheme base)
        (scheme write)
        (scheme cxr)
        (prefix (a-d disk config) disk:) ; AAN TE PASSEN Zorg dat deze library de (Hoofdstuk14 Oplossingen cached-disk-info) library importeert!
        (prefix (only (a-d disk _9-cached-disk-wbwa-info) cache-info reset-cache-info!) disk:) ; We hebben hier enkel deze twee functies nodig
        (prefix (a-d disk file-system) fs:)
        (prefix (a-d file sequential output-file) out:)
        (prefix (a-d file sequential input-file) in:)
        (only (scheme time) current-second) ; Om de tijd te meten
        (only (racket base) format)) ; Functie om te tonen hoe lang onze test duurt

(define (make-and-fill-file disk filename)
  (define outf (out:new disk filename))
  (do ((i 0 (+ i 1)))
    ((= i 25))
    (out:write! outf (current-second)))
  (out:close-write! outf))

(define (read-file disk filename)
  (let ((file (in:open-read! disk filename)))
    (display (format "Filename: ~a\n" filename))
    (do ()
      ((not (in:has-more? file)))
      (in:read file))
    (in:close-read! file)))

(define (display-cache-info disk)
  (let* ((cache-info (disk:cache-info disk))
         (requested-reads (car cache-info))
         (effective-reads (cadr cache-info))
         (requested-writes (caddr cache-info))
         (effective-writes (cadddr cache-info)))
    (display "Cache usage statistics:\n")
    (display "- Requested reads: ") (display requested-reads) (newline)
    (display "- Effective reads: ") (display effective-reads) (newline)
    (display "- Requested writes: ") (display requested-writes) (newline)
    (display "- Effective writes: ") (display effective-writes) (newline)
    (newline)))

(define (run-test disk nr-of-files)
  
  (define start-time (current-second))
  
  (display "Formatting the disk...") (newline)
  (fs:format! disk)
  (newline)
  
  (display-cache-info disk) (disk:reset-cache-info! disk)
  
  (display (format "Creating and filling ~a files...\n" nr-of-files))
  (do ((i 0 (+ i 1)))
    ((= i nr-of-files))
    (make-and-fill-file disk (format "testfile~a" i)))
  (newline)
  
  (display-cache-info disk) (disk:reset-cache-info! disk)
  
  (display "Directory contents:\n")
  (display (fs:ls disk)) (newline)
  (newline)
  
  (display-cache-info disk) (disk:reset-cache-info! disk)

  (display (format "Opening and reading 1st file ~a times...\n" nr-of-files))
  (do ((i 0 (+ i 1)))
    ((= i nr-of-files))
    (read-file disk "testfile0"))
  (newline)

  (display-cache-info disk) (disk:reset-cache-info! disk)
  
  ; NOTE: this test might fail when using cached-disk-info.rkt when too many blocks remain locked in cache!
  ; This can be fixed by either increasing the cache size or by altering input-file.rkt
  ; to effectively write header and buffer blocks back to disk once you're done reading them.
  ; That way, more blocks will become unlocked in the cache and more eviction can occur.
  ; However, this is a messy process and not neccesarily the correct solution...
  ; Therefore, look at cached-disk-wbwa.rkt using an alternative implementation of cached-disk,
  ; where the locking system has been removed so that no changes to input-file ADT are required! 
  (display (format "Opening and reading ~a files...\n" nr-of-files))
  (do ((i 0 (+ i 1)))
    ((= i nr-of-files))
    (read-file disk (format "testfile~a" i)))
  (newline)
  
  (display-cache-info disk) (disk:reset-cache-info! disk)
 
  (display (format "Total test running time: ~a seconds" (- (current-second) start-time))))

(define d (disk:new "MyDisk"))

(run-test d 10)
(disk:unmount d)