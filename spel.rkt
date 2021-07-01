;;;;;;;;;;;;;;--------------- Spel opstarten ------------;;;;;;;;;;;;;;;;;
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                     Abdullah Sabaa Allil                        *-*-
;-*-*                                                                 *-*-
;-*-*                      Programmeerproject 1                       *-*-
;-*-*                           2020-2021                             *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(#%require (only racket random error))
(#%require "Graphics.rkt")

(load "constanten.rkt")
(load "helpers.rkt")

(load "adt-positie.rkt")
(load "adt-raket.rkt")
(load "adt-kogel.rkt")
(load "adt-powerup.rkt")
(load "adt-alien.rkt") 
(load "adt-alienvloot.rkt") 
(load "adt-explosie.rkt") 
(load "adt-level.rkt") 
(load "adt-teken.rkt") 
(load "adt-spel.rkt") 
(load "adt-levens.rkt") 
(load "adt-scorebord.rkt") 
(load "adt-parent-actieve-powerup.rkt") 
(load "adt-actieve-powerups.rkt") 
(load "adt-actieve-alienvloot-freezer.rkt") 
(load "adt-actieve-raket-levenshersteller.rkt") 
(load "adt-actieve-raketscutum.rkt") 
(load "adt-actieve-scoreverdubbelaar.rkt") 
(load "adt-actieve-super-kogel.rkt") 
(load "adt-actieve-torpedo.rkt") 
(load "adt-botsingstoestand.rkt")


;; het spel opstarten
(define spel (maak-adt-spel))
((spel 'start))
