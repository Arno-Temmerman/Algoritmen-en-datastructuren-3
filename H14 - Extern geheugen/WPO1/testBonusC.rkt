#lang r7rs
(import (scheme base)
        (scheme write)
        (prefix (a-d disk config) disk:)
        (prefix (a-d disk _bonusC-file-system) fs:)) ; importeer hier je aangepaste file-system

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

; Test scenario
(define (run-test d)
  (display "No. files on disk d: ")
  (display (fs:number-of-files d))
  (newline)
  (display "ls: ")
  (display (fs:ls d))
  (newline)
  (display "(= (length (fs:ls d)) (fs:number-of-files d)): ")
  (display (= (length (fs:ls d)) (fs:number-of-files d)))
  (newline))

;;;

(define d (disk:new "test"))
(fs:format! d)                    ;; Create filesystem on disk
(touch-n d 22)                    ;; Register some "empty" files in the directory 
(run-test d)                      ;; Run test scenario
(rm-files d (list "6" "11" "15")) ;; Unregister some files
(run-test d)                      ;; Run test scenario again
(disk:unmount d)