#!/usr/bin/env racket

#lang racket
(require racket/gui/base)
(require 2htdp/batch-io)
(require racket/date)

; todo: 
;   need to make a key
;   enter keypress functionality for update page? 
;   need to add scrolling functionality

(define (create-frame n [a '(left top)])
  (new frame%
    [label n]
    [width 500]
    [height 500]
    [alignment a]))

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

(define KEY "asjeptmvntufj56")
(define FILE "impasse.txt")

; structure website name with the data to be encrypted (username, pass, etc)    
; the website is a string and the entries for it is a list of strings
(struct entry (website info) #:prefab) 

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
                       (when (equal? (send e get-event-type) 'text-field-enter) 
                         (if (equal? (send elt get-value) KEY) 
                           (begin (send (menu) show #t)
                                  (send protection show #f))
                           (message-box "Error" "Invalid Password" protection '(stop ok))))))]) 
    protection))

(define (menu)
  (let* ([opening-frame (create-frame "The choice is yours" '(center top))]
         [new-pass (create-button "New" opening-frame (swap-frames opening-frame (create-entry)))]
         [update-pass (create-button "Update" opening-frame (swap-frames opening-frame (update)))]
         [view-pass (create-button "View" opening-frame (swap-frames opening-frame (view)))])
    opening-frame))

(define (create-entry)
  (let* ([inputs (create-frame "Create an Entry")]
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
         [view-panel (new vertical-panel% [parent display-frame]
                                 [style (list 'vscroll)]
                                 [alignment (list 'left 'top)]
                                 [horiz-margin 10]
                                 [min-width (send display-frame get-width)]
                                 [min-height (send display-frame get-height)])]
         [l (read-passwords FILE)])
    (for/list ([i (in-range (length l))]) 
      (new message% [label (string-append (entry-website (list-ref l i)) ":")] 
                    [parent view-panel] 
                    [min-height 25]
                    [vert-margin 0])
      (new button% [label (string-trim (list-ref (string-split (entry-info (list-ref l i)) "password") 0))]
                   [parent view-panel]
                   [min-height 25]
                   [vert-margin 0]
                   [horiz-margin 15])
      (new button% [label (string-trim (string-append "password" (list-ref (string-split (entry-info (list-ref l i)) "password") 1)))]
                   [parent view-panel]
                   [min-height 25]
                   [vert-margin 0]
                   [horiz-margin 15]
                   [callback (lambda (elt e)
                                (send the-clipboard set-clipboard-string 
                                  (substring (list-ref (string-split (entry-info (list-ref l i)) "password") 1) 4)
                                    (send e get-time-stamp)))]))
    (create-button "Back" display-frame (lambda (elt e) 
                                          (send (menu) show #t)
                                          (send display-frame show #f)))
    display-frame))

; This reads the file as bytes, and decrypts only the info part of the entry
(define (read-passwords file)
  (let* ([in (open-input-file file)]
         [data (read in)])
    (close-input-port in)
    (map (lambda (i) (entry (entry-website i) (bytes->string/utf-8 (xor-en/decrypt (entry-info i) KEY)))) data)))

(define (update)
  (let* ([update-frame (create-frame "Make Your Changes")]
         [website (new choice% [label "Site Name:"]
                               [choices (site-list (read-passwords FILE))]
                               [parent update-frame])]
         [username (create-text-field "Username" update-frame (lambda (elt e) e))]
         [password (create-text-field "Pass it here" update-frame (lambda (elt e) e))]
         [enter-butt (create-button "Enter" update-frame (lambda (elt e)
                                                           (update-entry FILE (send website get-string-selection)
                                                                              (send username get-value)
                                                                              (send password get-value))
                                                           (send (view) show #t)
                                                           (send update-frame show #f)))])
    update-frame))

(define (backup file)
  (let* ([curr-date (string-replace (string-replace (date->string (current-date) #t) "," "") " " "-")])
    (copy-file file (string-append "backup/" file "-" curr-date))))

; update callback for updating username and password of a website
(define (update-entry file site user password)
  (backup file)
  (let* ([password-list (read-passwords file)]
         [out (open-output-file file #:exists 'replace)]
         [en (entry site (string-append "\tusername:  " user 
                                        "\n\tpassword:   " password))])
    (for ([i password-list])
      (when (equal? site (entry-website i)) 
        (set! password-list (remove (entry site (entry-info i)) password-list))))
    (set! password-list (append password-list (list en)))
    (write (map (lambda (i) (entry (entry-website i) (xor-en/decrypt (string->bytes/utf-8 (entry-info i)) KEY))) password-list) out)
    (close-output-port out)))

; For the update page choices, gives a list of websites
(define (site-list l)
  (if (empty? l) (list "nothing to update")
    (for/list ([i l])
      (entry-website i))))

; struct -> string
(define (style-printing s)
  (string-append "website: " (entry-website s) "\n   " (entry-info s) "\n"))

; function for reading/writing to file 
(define (write-data file site user pass)
  (let ([password-list (call-with-input-file file (lambda (in) (read in)))]
        [out (open-output-file file #:exists 'replace)]
        [en (entry (send site get-value)
                   (xor-en/decrypt 
                     (string->bytes/utf-8 
                       (string-append "\tusername:  " (send user get-value) 
                                      "\n\tpassword:   " pass)) KEY))])
    (backup file)
    (write (append password-list (list en)) out)
    (close-output-port out)))

; For new entry writing data on the enter keyboard press
(define (on-enter-key site user frame) 
  (lambda (elt e)
    (when (equal? (send e get-event-type) 'text-field-enter)
      (write-data FILE site user (send elt get-value))
      (send (view) show #t)
      (send frame show #f))))

(send (pass-protected) show #t)
