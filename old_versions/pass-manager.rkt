#!/usr/bin/env racket

#lang racket/base
(require racket/gui)
(require 2htdp/batch-io)

; todo: 
;   add a password at the beginning, to get into the program
;   check if the website that you are adding already exists, append info if it does
;   use the key from the beginning to encrypt data
;   only encrypt the data and not the website name
;   create frame as you go instead of having it sit around.

(define (create-frame n)
  (new frame%
    [label n]
    [width 500]
    [height 500]))

(define protection (create-frame "Try Me"))
(define input-frame (create-frame "Pass management"))
(define opening-frame (create-frame "The choice is yours"))
;(define display-frame (create-frame "Behold"))

; create a text-field for user input of password
(define (create-text-field n p c [e #t] [iv ""]) ;[e #t]
  (new text-field% 
       [label (string-append n ":")]
       [parent p]
       [callback c]
       [init-value iv]
       [enabled e]
       [min-width 250]
   	 	 [min-height 25]))

; for the opening screen, user choices
; (define (create-button n p c)
(define (create-button n p [c (lambda (a b) a)])
  (new button%
    [label n]
    [parent p]
    [callback c]
    [min-width 60]
   	[min-height 25]
    [vert-margin 5]
    [horiz-margin 5]))

; Testing the encryption
;(displayln (xor-en/decrypt (handle-file "test") key))
;(displayln (xor-en/decrypt (xor-en/decrypt (handle-file "test") key) key))

; structure website name with the data to be encrypted (username, pass, etc)    
; the website is a string and the entries for it is a list of strings
(struct entry (website info) #:prefab) ;#:transparent)

(define key "asjeptmvntufj56")
(define FILE "testing")

; xor encryption
(define (xor-en/decrypt str k) 
  (let* ([ktb (string->bytes/utf-8 k)])
    (list->bytes (for/list ([i (in-naturals 0)] [j str]) (bitwise-xor (bytes-ref ktb (modulo i (bytes-length ktb))) j)))))

;(define (read-passwords file) (call-with-input-file file 
;[fs (- (file-size file) 1)]
;[data (read-bytes fs in)])
;(map (lambda (i) (display (entry-info i))) l)
; This reads the file as bytes, and decrypts only the info part of the entry
(define (read-passwords file) 
  (let* ([in (open-input-file file)]
         [data (read in)])
    (close-input-port in)
    (map (lambda (i) (entry (entry-website i) (bytes->string/utf-8 (xor-en/decrypt (entry-info i) key)))) data)))

;(read-passwords FILE)

; struct -> string
(define (style-printing s)
  (string-append "website: " (entry-website s) "\n   " (entry-info s) "\n"))

; For the callback to swap frames, and for enter
(define (swap-frames from-frame to-frame) 
  (lambda (elt e)
    ;(when (or (equal? (send e get-event-type) 'text-field-enter) (equal? elt enter-butt))
      (send to-frame show #t)
      (send from-frame show #f)))

; list -> string
; callback for setting display-passwords text-field, takes a list after the file has been processed
(define (display-cb l from-f)
  (lambda (elt e)
    ; read in the file, then  print everything nice and proper
    ; if the button is the View button, or if it is an enter from the input frame...then read from file
    (when (equal? elt view-pass) ;(or (equal? (send e get-event-type) 'text-field-enter) (equal? elt view-pass))
      (send display-passwords set-value "Changing it, to something else \n see here")
      (swap-frames from-f (create-frame "Behold")))))
      ;(for/list ([i (in-range (length l))]) (send display-passwords set-value (style-printing (list-ref l i))))

; testing
;(display (entry-info (first test-list)))
;(display (string? (entry-info (first test-list))))
;(define test-list (read-passwords FILE))
;(display (style-printing (first test-list)))
(display-cb (read-passwords FILE))

; Buttons for choices page
(define new-pass-butt (create-button "New" opening-frame (swap-frames opening-frame input-frame)))
(define update-pass-butt (create-button "Update" opening-frame (swap-frames opening-frame input-frame)))
(define view-pass (create-button "View" opening-frame))
;(define view-pass (create-button "View" opening-frame (swap-frames opening-frame display-frame)))

(define (load-passwords file) (call-with-input-file file (lambda (in) (read in))))

; make this check to see if the (entry website) already exists
; if it does, then just append the value to the end of the info field
; callback function for a file to write/read data from
(define (write-data-on-enter file from-frame to-frame) 
  (lambda (elt e)
    (when (or (equal? (send e get-event-type) 'text-field-enter) (equal? elt enter-butt))
      (unless (equal? elt password)
        (let ([password-list (load-passwords file)]
              [out (open-output-file file #:exists 'replace)]
              [en (entry (send site-name get-value) 
                         (xor-en/decrypt 
                           (string->bytes/utf-8 
                             (string-append "username:" (send username get-value) 
                                            " password:" (send pass-entry get-value))) key))])
          (write (append password-list (list en)) out)
          (close-output-port out)))
      (send to-frame show #t)
      (send from-frame show #f))))

(define password (create-text-field "Password" protection (write-data-on-enter "testing" protection opening-frame)))
(define site-name (create-text-field "Site Name" input-frame (lambda (a b) a))) 
(define username (create-text-field "Username" input-frame (lambda (a b) a)))
(define pass-entry (create-text-field "Pass it here" input-frame (write-data-on-enter "testing" input-frame (create-frame "Behold")))) ;display-frame)))
(define enter-butt (create-button "Enter" input-frame (write-data-on-enter "testing" input-frame (create-frame "Behold")))) ;display-frame)))
; make text-field to display current passwords with enabled set to false
(define display-passwords (create-text-field "Display Here" (create-frame "Behold") ;(swap-frames opening-frame display-frame) #f))
                            (display-cb (read-passwords FILE) (create-frame "Behold")) #f "Hello World")) 
;(define (display-passwords) (create-text-field "Display Here" display-frame (lambda (a b) a) #f))
   
; uncomment everything below after writing and testing encryption
(send protection show #t)
;(display (load-passwords "testing"))
