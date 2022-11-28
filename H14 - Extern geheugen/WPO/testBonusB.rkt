#lang r7rs
(import (scheme base)
        (scheme write)
        (prefix (a-d disk config) disk:)
        (prefix (a-d disk _bonusB-file-system) fs:)) ; importeer hier je aangepaste file-system

;;; Some auxiliary procedures

; Register an "empty" file
(define (touch disk name)
  (fs:mk disk name fs:null-block))

; Touch the directory n times
(define (touch-n disk n)
  (let loop
    ((i 0))
    (unless (= i n)
      (touch disk (number->string i))
      (loop (+ i 1)))))

; Unregistering some files
(define (rm-files disk names)
  (for-each (lambda (name)
              (fs:rm disk name)
              (display (string-append "Unregistered file: " name))
              (newline))
            names))

; Reading slot counters from directory list
(define (read-slot-ctrs d)
  (define meta (fs:read-meta-block d))
  (let loop-dir
    ((bptr (fs:directory meta)))
    (unless (fs:null-block? bptr)
      (let ((blck (disk:read-block d bptr)))
        (display "Slots occupied in directory block ")
        (display bptr)
        (display ": ")
        (display (fs:slot-ctr blck))
        (newline)
        (loop-dir (fs:next-bptr blck))))))

;;; 

(define d (disk:new "test"))
(fs:format! d)               ;; Create filesystem on disk
(touch-n d 42)               ;; Register some "empty" files in the directory
(read-slot-ctrs d)           ;; Read the occupation of each directory block
(define rm-names (list "1" "10" "11" "12" "25" "34" "38"))
(rm-files d rm-names)        ;; Unregister some files
(read-slot-ctrs d)           ;; See how the occupation is altered
(disk:unmount d)