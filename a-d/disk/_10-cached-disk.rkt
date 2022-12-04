#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                          Cached Disk                            *-*-
;-*-*                                                                 *-*-
;-*-*       Theo D'Hondt - Wolfgang De Meuter - Youri Coppens         *-*-
;-*-*         1993 - 2022 Department of Informatics (DINF)            *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (cached-disk)
  (export block-size disk-size
          block-ptr-size block-idx-size real32 real64
          block? disk position 
          new mount unmount name disk-size
          read-block write-block!
          decode-byte encode-byte! 
          decode-fixed-natural encode-fixed-natural!
          decode-arbitrary-integer encode-arbitrary-integer! 
          integer-bytes natural-bytes
          decode-real encode-real! 
          decode-string encode-string!
          decode-bytes encode-bytes!)
  (import (scheme base)
          (a-d scheme-tools)
          (prefix (a-d disk disk) disk:))
  (begin
    (define real32 disk:real32)
    (define real64 disk:real64)
 
    (define block-size disk:block-size)
    (define disk-size  disk:disk-size)
    (define block-ptr-size disk:block-ptr-size)
    (define block-idx-size disk:block-idx-size)
 
    (define cache-size 10)

    (define cache-parts 3)
    (define cache-part-size (quotient cache-size cache-parts))

    (define (cache:part bptr) ; determine in which part of the cache a block should be put
      (modulo bptr cache-parts))
 
    (define (cache:in-last-part? bptr)
      (= (cache:part bptr)
         (- cache-parts 1)))

    (define (cache:start-offs bptr) ; start-idx of part
      (* cache-part-size (cache:part bptr)))
 
    (define (cache:end-offs bptr) ; end-idx of part in cache, ensures last part goes until the end of the cache vector
      (if (cache:in-last-part? bptr)
          cache-size
          (+ (cache:start-offs bptr)
             cache-part-size)))
 
    (define (cache:new)
      (make-vector cache-size '()))
 
    (define (cache:get cche indx)
      (vector-ref cche indx))
 
    (define (cache:put! cche indx blck)
      (vector-set! cche indx blck))
 
    (define (cache:find-free-index cche bptr start-offs end-offs)
      (define oldest-time (current-time))
      (define oldest-indx -1)
      (define (traverse indx)
        (if (< indx end-offs) ; stop at the end of cache part
            (let 
                ((blck (cache:get cche indx)))
              (if (null? blck)
                  indx
                  (let ((lckd (locked? blck)))
                    (if (not lckd)
                        (let 
                            ((stmp (time-stamp blck)))
                          (when (time<? stmp oldest-time)
                            (set! oldest-time stmp)
                            (set! oldest-indx indx))))
                    (traverse (+ indx 1)))))
            (if (negative? oldest-indx)
                (error "cache full" cche)
                oldest-indx)))
      (traverse start-offs)) ; start at the start of cache part
 
    (define (cache:find-block cche bptr start-offs end-offs)
      (define (position-matches? blck)
        (and (not (null? blck))
             (= (position blck) bptr)))
      (let traverse
        ((indx start-offs) ; start at the start of cache part
         (blck (cache:get cche start-offs)))
        (cond ((position-matches? blck)
               blck)
              ((< (+ indx 1) end-offs) ; stop at the end of cache part
               (traverse (+ indx 1) 
                         (cache:get cche (+ indx 1))))
              (else
               '() ))))
 
    (define-record-type cdisk
      (make-cdisk v d)
      disk?
      (v disk-cache)
      (d real-disk))
 
    (define (new name)
      (make-cdisk (cache:new) (disk:new name)))
 
    (define (mount name)
      (make-cdisk (cache:new) (disk:new name)))
 
    (define (name cdsk)
      (disk:name (real-disk cdsk)))
 
    (define (unmount cdsk) 
      (define vctr (disk-cache cdsk))
      (define (traverse indx)
        (if (< indx cache-size)
            (let ((blck (cache:get vctr indx)))
              (cond
                ((not (null? blck))
                 (if (dirty? blck)
                     (disk:write-block! (block blck)))
                 (invalidate! blck)))
              (traverse (+ indx 1)))))
      (traverse 0)
      (disk:unmount (real-disk cdsk)))
 
    (define (write-block! blck) 
      (define cche (disk-cache (disk blck)))
      (if (not (valid? blck))
          (error "invalidated block(write-block!)" blck))
      (locked! blck #f)) ; no write, that's the whole point!
 
    (define (read-block cdsk bptr)
      (define cche (disk-cache cdsk))
      (define start-offs (cache:start-offs bptr))
      (define end-offs (cache:end-offs bptr))
      (define blck (cache:find-block cche bptr start-offs end-offs))
      (if (null? blck)
          (let*
              ((indx (cache:find-free-index cche start-offs end-offs))
               (blck (cache:get cche indx)))
            (when (not (null? blck))
              (if (dirty? blck)
                  (disk:write-block! (block blck)))
              (invalidate! blck))
            (set! blck (make-block cdsk (disk:read-block (real-disk cdsk) bptr)))
            (cache:put! cche indx blck)))
      (locked! blck #t)
      blck)
 
    (define-record-type cblock
      (make d l t i b)
      block?
      (d dirty? dirty-set!)
      (l locked? locked!)
      (t time-stamp time-stamp!)
      (i disk)
      (b block block!))
 
    (define (make-block cdsk blck)
      (make #f #t (current-time) cdsk blck))
 
    (define (dirty! blck)
      (dirty-set! blck #t))
 
    (define (invalidate! blck)
      (block! blck '()))
 
    (define (valid? blck)
      (not (null? (block blck))))
 
    (define (position blck) 
      (if (not (valid? blck))
          (error "invalidated cblock(position)" blck))
      (disk:position (block blck)))
 
    (define integer-bytes disk:integer-bytes)
 
    (define natural-bytes disk:natural-bytes)
 
    (define (make-cached-encoder proc)
      (lambda args
        (define blck (car args))
        (if (not (valid? blck))
            (error "invalidated cblock(cached version of encoder)" blck))
        (time-stamp! blck (current-time))
        (dirty! blck)
        (apply proc (cons (block blck)  (cdr args)))))
 
    (define encode-byte! (make-cached-encoder disk:encode-byte!))
    (define encode-fixed-natural! (make-cached-encoder disk:encode-fixed-natural!))
    (define encode-arbitrary-integer! (make-cached-encoder disk:encode-arbitrary-integer!))
    (define encode-real! (make-cached-encoder disk:encode-real!))
    (define encode-string! (make-cached-encoder disk:encode-string!))
    (define encode-bytes! (make-cached-encoder disk:encode-bytes!))
 
    (define (make-cached-decoder proc)
      (lambda args
        (define blck (car args))
        (if (not (valid? blck))
            (error "invalidated cblock(cached version of decoder)" blck))
        (time-stamp! blck (current-time))
        (apply proc (cons (block blck)  (cdr args)))))
 
    (define decode-byte (make-cached-decoder disk:decode-byte))
    (define decode-fixed-natural (make-cached-decoder disk:decode-fixed-natural))
    (define decode-arbitrary-integer (make-cached-decoder disk:decode-arbitrary-integer))
    (define decode-real (make-cached-decoder disk:decode-real))
    (define decode-string (make-cached-decoder disk:decode-string))
    (define decode-bytes (make-cached-decoder disk:decode-bytes))))
