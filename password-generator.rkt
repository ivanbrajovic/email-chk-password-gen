#lang racket/gui
(define get-char (λ(n) (integer->char n)))
(define set-rand-char(λ(n)(list-ref n (random (length n)))))
(define char-codes-range
  (λ(from to)
    (map get-char (range from to))))

(define extract-items
      (λ(local-characters)
        (λ(all-available-characters)
          (filter (λ(x)(not(index-of local-characters x))) all-available-characters))))
(define concat-items
      (λ(local-characters)
        (λ(all-available-characters)
          (append local-characters all-available-characters))))

(define generate-chars
  (λ(l)(λ(n)
      (list->string
       (map (λ(x)(set-rand-char n)) (range l))))))

(define chars
  (λ(ref)
    (hash-ref(hash
     'spc-1 (char-codes-range 33 48)
     'num   (char-codes-range 48 58)
     'spc-2 (char-codes-range 58 65)
     'upp   (char-codes-range 65 91)
     'spc-3 (char-codes-range 91 97)
     'low   (char-codes-range 97 123)
     'spc-4 (char-codes-range 123 127)
     )ref)))

(define extract-spc-1 (extract-items (chars 'spc-1)))
(define extract-num   (extract-items (chars 'num)))
(define extract-spc-2 (extract-items (chars 'spc-2)))
(define extract-upp   (extract-items (chars 'upp)))
(define extract-spc-3 (extract-items (chars 'spc-3)))
(define extract-low   (extract-items (chars 'low)))
(define extract-spc-4 (extract-items (chars 'spc-4)))
(define concat-spc-1  (concat-items (chars 'spc-1)))
(define concat-num    (concat-items (chars 'num)))
(define concat-spc-2  (concat-items (chars 'spc-2)))
(define concat-upp    (concat-items (chars 'upp)))
(define concat-spc-3  (concat-items (chars 'spc-3)))
(define concat-low    (concat-items (chars 'low)))
(define concat-spc-4  (concat-items (chars 'spc-4)))


(define all (char-codes-range 33 127))
(define l->s (λ(n)(list->string n)))


(define validate-email
  (λ(e)
    (let* ([pattern-A  "[^]\"()@\\,.:;<> []"]
           [pattern-B  "[.][^.@]" ]
           [pattern-join
                (λ(time)(string-join (list "(" pattern-A "|" pattern-B ")" time )""))]
           [pattern-n "[a-zA-Z]"]
           [pattern-i "[a-zA-Z0-9]"]
           [pattern-C
            (string-join
             (list
              "("
              pattern-n "|"
              pattern-n "[a-zA-Z-]*" pattern-i "|"
              pattern-n "[a-zA-Z0-9-]+" pattern-i "|"
              "[0-9][a-zA-Z0-9-]*" pattern-n ")")"")])
    (if(regexp-match
        (string-join
         (list
           "^(("
           pattern-A "|" pattern-A (pattern-join "+")
           ")|"
           "(\"[^\"]*\""
           "|"
           "("
           pattern-A (pattern-join "*")
           "([.]+\"[^\"]*\"[.])*"
           (pattern-join "+")
           ")))"
           "@"
           "("
           pattern-C
           "([.]"
           ")*)+"
           pattern-C
           "$")"") e)
       "E-mail адреса је исправна"
       "E-mail адреса није исправна"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*************************************************;
    
;;;gui part
;;UNSAFE this will mutate global ````all```` '(characters)
(define all-available-password-chars
  (λ(affected-item)
    (set! all (affected-item all))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define frame
  (new frame%
    [label "E-mail валидатор и генератор лозинке"]
    (style (list 'no-resize-border))
    [width 440]
    [height 480]))
(define panel
  (new panel%
       [parent  frame]
       [min-width 400]
       [stretchable-width #f]))

(define container
  (new vertical-panel%
       [parent panel]
       [min-width 400]
       [stretchable-width #f]
       ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*************************************************;
(define group
  (λ(l x h)
    (new  group-box-panel%
    [label l]
    [parent x]
    [min-height h]
    [stretchable-height #f])))


(define email-container
  (group "EMAIL ВАЛИДАТОР" container 50))
(define check-email
  (group "провери E-mail" email-container 50))
(define password-container
  (group "ГЕНЕРАТОР ЛОЗИНКЕ" container 300))
(define filter-choose-container
  (group "укључи / искључи карактере" password-container 100))
(define password-length
  (group "дужина лозинке" password-container 50))
(define wrapper-input
  (group "укључи / искључи карактере уносом" password-container 50))
(define generate
  (group "прикажи" password-container 133))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*************************************************;
;;; EMAIL CHECKER
(define email-checkercontainer
  (new horizontal-panel%
       [parent check-email]
      ))
(define email-checker
  (new text-field%
       [label "унеси E-mail"]
       [parent email-checkercontainer]))
(define btn-email-checker
  (new button%
       	[label "провери"]	 
        [parent  email-checkercontainer]
        [callback
         (λ(a n)
           (message-box
            "валидатор E-mail адресе"
            (validate-email (send email-checker get-value))
            frame
            (list 'ok))
            )]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*************************************************;
(define container-check
  (new horizontal-panel%
       [parent filter-choose-container]))
(define  choose-container-left
  (new vertical-panel%
       [parent container-check]
       [min-width 200]
       [stretchable-width #f]))
(define  choose-container-right
  (new vertical-panel%
       [parent container-check]
       [min-width 200]
       [stretchable-width #f]))
(define charsets-left
  (λ()
   (new horizontal-panel%
       [parent choose-container-left])))
(define charsets-right
  (λ()
   (new horizontal-panel%
       [parent choose-container-right])))
(define select-charset
  (λ(x l  message-enabled? to-concat to-extract)
    (new check-box%
       [parent x]
       [callback
        (λ(a n)
          (let ([bool (send a get-value)])
            (send (message-enabled?) enable bool)
            (if bool
                (all-available-password-chars to-concat)
                (all-available-password-chars to-extract))))]
       [vert-margin 0]
       [label l]
       [min-width 100]
       [stretchable-width #f]
       [value #t])))
(define charset-message
  (λ(a n)
    (new message%
         [label a]	 
         [parent n]
         [enabled #t]
         [min-width 100]
         [stretchable-width #f]
         [font (send the-font-list find-or-create-font
           8 "Arial" 'default 'normal 'bold )])))

(define charset-1 (charsets-left))
(define charset-2 (charsets-left))
(define charset-3 (charsets-left))
(define charset-4 (charsets-left))
(define charset-5 (charsets-right))
(define charset-6 (charsets-right))
(define charset-7 (charsets-right))
(define select-characters-1
   (select-charset charset-1
   "спец. карактери 1"
    (λ() special-c-1-message)
    concat-spc-1
    extract-spc-1))

(define select-characters-2
  (select-charset charset-2
    "бројеви"
    (λ() nums-message)
    concat-num
    extract-num))
(define select-characters-3
  (select-charset charset-3
   "спец. карактери 2"
   (λ()special-c-2-message)
    concat-spc-2
    extract-spc-2))
(define select-characters-4
  (select-charset charset-4


    "велика словa"
   (λ()upper-message)
   concat-upp
   extract-upp))


(define select-characters-5
  (select-charset charset-5
   "спец. карактери 3"
   (λ()special-c-3-message)
   concat-spc-3
   extract-spc-3))
(define select-characters-6
  (select-charset charset-6
   "мала слова"
   (λ()lower-message)
   concat-low
   extract-low))

(define select-characters-7
  (select-charset charset-7
   "спец. карактери 4 "
    (λ()special-c-4-message)
    concat-spc-4
    extract-spc-4))

(define special-c-1-message
  (charset-message(l->s (chars 'spc-1)) charset-1))

(define nums-message
  (charset-message "0...9" charset-2))

(define special-c-2-message
  (charset-message(l->s (chars 'spc-2)) charset-3))

(define upper-message
  (charset-message  "A...Z" charset-4))

(define special-c-3-message
  (charset-message  (l->s (chars 'spc-3)) charset-5))


(define lower-message
  (charset-message "a...z" charset-6))

(define special-c-4-message
  (charset-message (l->s (chars 'spc-4)) charset-7))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*************************************************;
(define container-length
  (new horizontal-panel%
       [parent  password-length]))
(define length-indicator
  (new slider%
       [label "дужина"]
       [callback
        (λ(a n)
          (send combo-field set-string-selection
                 (number->string(send length-indicator get-value))))]
       [parent container-length]
       [min-value 8]
       [max-value 32]
       [init-value 8]
       ))
 (define combo-field
   (new choice%
       [label "постави дужину"]
       [parent container-length]
       [callback
        (λ(a n)
          (send length-indicator set-value
               (string->number (send combo-field get-string-selection))))]
       [choices (map (λ(n)(number->string n)) (range 8 33))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*************************************************;
(define container-check-inputs
  (new horizontal-panel%
       [parent wrapper-input]))

(define container-input
  (new horizontal-panel%
       [enabled #f]
       [parent wrapper-input]))
(define select-manual-input
  (new check-box%
       [parent container-check-inputs]
       [vert-margin 10]
       [label "ручно филтрирање"]
       [callback
        (λ(check-box event)
          (send container-input enable
                (send select-manual-input get-value)))]
       [value #f]
       ))

(define input-text
  (λ(l get-value)
    (new text-field%
       [parent container-input]
       [label l]
       [style (list 'single 'vertical-label)]
       [callback get-value]
   )))

(define include-textfield
  (input-text "укључи карактере"
  (λ(a n)
    (all-available-password-chars (concat-items (string->list (send a get-value)))))))

(define exclude-textfield
  (input-text "искљули карактере"
  (λ(a n)
    (all-available-password-chars (extract-items (string->list (send a get-value)))))
  ))
(define display-pass
  (new vertical-panel%
       [parent generate]))
(define show-password
  (new message%
       [parent display-pass] 
       [label "-"]
       [font (send the-font-list find-or-create-font
           12 "Arial" 'default 'normal 'bold )]
       [min-height 100]
       [min-width 400]
       [auto-resize #t]
       ))

(define copy-button
  (new button%
       [callback
        (λ(a n)
          (send the-clipboard set-clipboard-string
                (send show-password get-label) (current-milliseconds)))]
       [parent display-pass]
       [label "копирај"]
       [min-width 50]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*************************************************;
;; g e n e r a t e d    p a s s w o r d
(define generate-password
  (new button%
       [parent generate]
       [label "генериши"]
       [callback
        (λ(a n)
          (let* ([d (string->number (send combo-field get-string-selection))]
                [extract-password (generate-chars d)])
          (send show-password set-label
          (if
           (equal? '() all)
           "нисте изабрали ниједан карактер"
           (if (regexp-match " " (extract-password all))
               "искључите space карактер/е из мануелног филтера"
               (extract-password all))))
          ))]
       [style (list 'border 'multi-line)]
       [min-width 100]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*************************************************;
(send frame create-status-line)
(send frame set-status-text "© Ξενία applications 2020")
(send frame show #t)