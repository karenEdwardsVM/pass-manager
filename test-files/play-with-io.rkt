#!/usr/bin/env racket

#lang racket/base

;(require 2htdp/batch-io)
;
;(write-file "data" "hello out there")
;(write-file "data" "someone")
;(write-file "data" "anyone")
;(read-line "data")

(define out (open-output-file "test" #:exists 'append))
(display "website:\n" out)
(display "  username:\n" out)
(display "  pass:\n" out)
(close-output-port out)

(define in (open-input-file "test"))
(for ([l (in-lines in)]) (display (string-append l "\n")))
(close-input-port in)
