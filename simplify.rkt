#!/usr/bin/env racket

#lang racket/base
(require racket/gui)
(require 2htdp/batch-io)

; todo: 
;   add a password at the beginning, to get into the program
;   check if the website that you are adding already exists, append info if it does
;   use the key from the beginning to encrypt data
;   instead of text field for the display, use message

(define (create-frame n)
  (new frame%
    [label n]
    [width 500]
    [height 500]))

; create a text-field for user input of password
(define (create-text-field n p c [e #t] [iv ""] [h 25]) ;[e #t]
  (new text-field% 
       [label (string-append n ":")]
       [parent p]
       [callback c]
       [init-value iv]
       [enabled e]
       [min-width 200]
   	 	 [min-height h]))

(define (create-button n p c)
  (new button%
    [label n]
    [parent p]
    [callback c]
    [min-width 60]
   	[min-height 25]
    [vert-margin 5]
    [horiz-margin 5]))

(define key "asjeptmvntufj56")
(define FILE "testing")

; structure website name with the data to be encrypted (username, pass, etc)    
; the website is a string and the entries for it is a list of strings
(struct entry (website info) #:prefab) ;#:transparent)

; xor encryption
(define (xor-en/decrypt str k) 
  (let* ([ktb (string->bytes/utf-8 k)])
    (list->bytes (for/list ([i (in-naturals 0)] [j str]) (bitwise-xor (bytes-ref ktb (modulo i (bytes-length ktb))) j)))))

; For the callback to swap frames, and for enter
(define (swap-frames from-frame to-frame) 
  (lambda (elt e)
      (send to-frame show #t)
      (send from-frame show #f)))

(define (pass-protected)
  (let* ([protection (create-frame "Try Me")]
         [password (create-text-field "Password" protection 
                     (lambda (elt e) 
                       (displayln (send elt get-value))
                       (when (equal? (send e get-event-type) 'text-field-enter) 
                         (if (equal? (send elt get-value) key) 
                           (begin (send (choices) show #t)
                                  (send protection show #f))
                           (message-box "Error" "Invalid Password" protection '(stop ok))))))]) 
    protection))

(define (choices)
  (let* ([opening-frame (create-frame "The choice is yours")]
         [new-pass (create-button "New" opening-frame (swap-frames opening-frame (create-entry)))]
         [update-pass (create-button "Update" opening-frame (swap-frames opening-frame (create-entry)))]
         [view-pass (create-button "View" opening-frame (swap-frames opening-frame (view)))])
    opening-frame))

(define (create-entry)
  (let* ([inputs (create-frame "The choice is yours")]
         [website (create-text-field "Site Name" inputs (lambda (elt e) e))] 
         [username (create-text-field "Username" inputs (lambda (elt e) e))]
         [password (create-text-field "Pass it here" inputs (on-enter-key website username inputs))]
         [enter-butt (create-button "Enter" inputs (lambda (elt e) 
                                                     (send (view) show #t)
                                                     (send inputs show #f)
                                                     (write-data FILE website username (send password get-value))))])
    inputs))

(define (view)
  (let* ([display-frame (create-frame "Behold")]
         [l (read-passwords FILE)])
    (for/list ([i (in-range (length l))]) 
      ;(new message% [label (string-append (entry-website (list-ref l i)) " " (entry-info (list-ref l i)))] 
      ;(new message% [label (entry-website (list-ref l i))] [parent (display-frame)] [min-height 38]))
      (create-text-field (entry-website (list-ref l i)) display-frame (lambda (elt e) e) #t (entry-info (list-ref l i)) 38))
    display-frame))

; This reads the file as bytes, and decrypts only the info part of the entry
(define (read-passwords file) 
  (let* ([in (open-input-file file)]
         [data (read in)])
    (close-input-port in)
    (map (lambda (i) (entry (entry-website i) (bytes->string/utf-8 (xor-en/decrypt (entry-info i) key)))) data)))

;(read-passwords FILE) ; test

; struct -> string
(define (style-printing s)
  (string-append "website: " (entry-website s) "\n   " (entry-info s) "\n"))

; test for displaying
;(displayln (style-printing (list-ref (read-passwords FILE) 1)))

; function for reading/writing to file 
(define (write-data file site user pass)
  (let ([password-list (call-with-input-file file (lambda (in) (read in)))]
              [out (open-output-file file #:exists 'replace)]
              [en (entry (send site get-value)
                         (xor-en/decrypt 
                           (string->bytes/utf-8 
                             (string-append "username:  " (send user get-value) 
                                            "\npassword:   " pass)) key))])
          (write (append password-list (list en)) out)
          (close-output-port out)))
          ;(displayln (string-append "site: " (send site get-value)))
          ;(displayln (string-append "username: " (send user get-value)))
          ;(displayln (string-append "password: " pass))

; For new entry writing data on the enter keyboard press
(define (on-enter-key site user frame) 
  (lambda (elt e)
    (when (equal? (send e get-event-type) 'text-field-enter)
      (write-data FILE site user (send elt get-value))
      (send (view) show #t)
      (send frame show #f))))

(send (pass-protected) show #t)

