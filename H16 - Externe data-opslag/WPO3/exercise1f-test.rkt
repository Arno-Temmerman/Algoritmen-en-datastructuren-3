#lang r7rs

;; BE SURE TO UNCOMMENT either (a-d disk disk) or (a-d disk cached-disk-wbwa) in config.rkt (a-d disk config) 
;; AVOID (a-d disk cached-disk) as the block locking system might shoot you in the foot...

(import (prefix (a-d disk config) disk:)
        (prefix (a-d disk file-system) unix:)
        (prefix (Hoofdstuk16 Oplossingen schema+) scma:) ; Add variable max-attributes to Schema ADT
        (scheme base)
        (scheme write)
        (only (racket base) format))

;--------- Exercises HFST 16 -----------

;--------- Test (Ex. 16.1.f)-----------

(define disk (disk:new "HardDisk"))
(unix:format! disk)
;
; This schema should be accepted:
(define schemaWithinLimit (vector->list (make-vector scma:max-attributes '(string 1))))
(define scma1 (scma:new disk schemaWithinLimit))
(display (format "Schema with ~a attributes successfully created\n" scma:max-attributes))

; This schema should be refused:
(define schemaTooBig (vector->list (make-vector (+ scma:max-attributes 1) '(string 1))))

; We'll use a guard to capture the expected error (see section 4.2.7 of R7RS standard for more info)
(guard (res ((error-object? res) 
             (display "An error has occurred and has been captured!\n")
             (display (error-object-message res)))
            (else (display "Something odd was raised from scma:new\n")
                  (display res)
                  (newline)))
    (scma:new disk schemaTooBig) ;--> MUST GIVE AN ERROR!
    (display "If you see this line, you're doing something wrong!\n")) ; this display should not be printed in REPL

(disk:unmount disk) ;;  don't forget to clean-up after you're done (especially when using cached disks)