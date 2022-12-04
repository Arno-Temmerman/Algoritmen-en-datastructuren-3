#lang r7rs

(import (scheme base)
        (scheme write)
        (prefix (a-d disk config) disk:)
        (prefix (a-d file sequential input-file) in:)
        (prefix (a-d file sequential output-file) out:)
        (prefix (a-d disk file-system) fs:))

(define (cat disk file-name)
  (define file (in:open-read! disk file-name))
  (let loop ()
    (if (in:has-more? file)
        (begin (display (in:read file))
               (loop)))))

(define d (disk:new "My Computer"))
(fs:format! d)
(define f (out:new d "TestFile"))
(out:write! f 3.14)
(out:write! f 42)
(out:write! f "Done!")
(out:close-write! f)
(set! f (in:open-read! d "TestFile"))
(display (in:read f))(newline)
(display (in:read f))(newline)
(display (in:read f))(newline)
(in:close-read! f)

(cat d "TestFile")

(disk:unmount d)