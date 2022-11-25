#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*           Scheme Tools that should have been included           *-*-
;-*-*                                                                 *-*-
;-*-*               Wolfgang De Meuter - Youri Coppens                *-*-
;-*-*                   2022 Software Languages Lab                   *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library
  (scheme-tools)
  (export vector-map! vector-for-each+ bytevector-u8-map! random-inbetween random-integer make-2D-vector ij! ij?
          current-time time<? time<=?
          for-all exists fold-right fold-left
          remove remv remq
          memp remp
          filter)
  (import (scheme base)
          (srfi 27)
          (srfi 19))
  (begin
    (define (random-inbetween l r)
      (+ l (random-integer (+ (- r l) 1))))
 
    (define (vector-map! v f)
      (define last (- (vector-length v) 1))
      (let loop
        ((i 0))
        (vector-set! v i (f i (vector-ref v i)))
        (if (< i last)
            (loop (+ i 1))))
      v)
 
    (define (vector-for-each+ v f)
      (define last (- (vector-length v) 1))
      (define res (make-vector (vector-length v) '()))
      (let loop 
        ((i 0))
        (vector-set! res i (f i (vector-ref v i)))
        (if (< i last)
            (loop (+ i 1))))
      res)
 
    (define (bytevector-u8-map! v f)
      (define last (- (bytevector-length v) 1))
      (let loop
        ((i 0))
        (bytevector-u8-set! v i (f i (bytevector-u8-ref v i)))
        (if (< i last)
            (loop (+ i 1))))
      v)
 
    (define (make-2D-vector n m proc)
      (define res (make-vector n '()))
      (vector-map! 
       res 
       (lambda (i el) 
         (define row (make-vector m '()))
         row))
      (vector-map!
       res
       (lambda (i row)
         (vector-map!
          row
          (lambda (j el)
            (proc i j)))
         row)))
 
    (define (ij? v i j)
      (vector-ref (vector-ref v i) j))
    (define (ij! v i j a)
      (vector-set! (vector-ref v i) j a))

    ;;; The following procedures were part of default R6RS libraries which have been ommited in R7RS-small

    (define (for-all proc list)
      (if (null? list)
          #t
          (let loop ((list list))
            (let ((next (cdr list)))
              (cond
                ((null? next) (proc (car list))) 
                ((proc (car list)) (loop next))
                (else #f))))))

    (define (exists proc list)
      (if (null? list)
          #f
          (let loop ((list list))
            (let ((next (cdr list)))
              (if (null? next)
                  (proc (car list))
                  (or (proc (car list))
                      (loop next)))))))

    (define (fold-right combine nil lst)
      (let recur ((curr-lst lst))
        (if (null? curr-lst)
            nil
            (combine (car curr-lst) (recur (cdr curr-lst))))))

    (define (fold-left combine nil lst)
      (let loop ((accum nil)
                 (curr-lst lst))
        (if (null? curr-lst)
            accum
            (loop (combine (car curr-lst) accum)
                  (cdr curr-lst)))))

    (define-syntax define-remove-like
      (syntax-rules ()
        ((define-remove-like ?name ?equal?)
         (define (?name obj lst)
           (let recur ((curr-lst lst))
             (cond ((null? curr-lst) '())
                   ((?equal? obj (car curr-lst))
                    (recur (cdr curr-lst)))
                   (else
                    (let ((r (recur (cdr curr-lst))))
                      (if (eq? r (cdr curr-lst))
                          list
                          (cons (car curr-lst) r))))))))))

    (define-remove-like remove equal?)
    (define-remove-like remv eqv?)
    (define-remove-like remq eq?)

    (define (memp proc list)
      (let loop ((list list))
        (cond ((null? list) #f)
              ((proc (car list)) list)
              (else (loop (cdr list))))))
    
    (define (remp proc list)
      (let recur ((list list))
        (cond ((null? list) '())
              ((proc (car list))
               (recur (cdr list)))
              (else
               (let ((r (recur (cdr list))))
                 (if (eq? r (cdr list))
                     list
                     (cons (car list) r)))))))
    
    (define (filter proc list)
      (let loop ((list list))
        (cond ((null? list) '())
              ((proc (car list))
               (let ((r (loop (cdr list))))
                 (if (eq? r (cdr list))
                     list
                     (cons (car list) r))))
              (else
               (loop (cdr list))))))
    ))