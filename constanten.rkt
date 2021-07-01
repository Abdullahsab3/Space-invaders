(define cel-breedte-px 20)
(define cel-hoogte-px 20)

(define spel-breedte 40)
(define spel-hoogte 30)

(define max-fps 50)

(define venster-breedte-px (* cel-breedte-px spel-breedte))
(define venster-hoogte-px (* cel-hoogte-px spel-hoogte))

(define rechterzijkant (- spel-breedte 1))
(define linkerzijkant 0)
(define bovenkant 0)
(define bodem (- spel-hoogte 1))

(define initiele-alienvloot-snelheid 225)
(define vloot-stil-voor-10-secs 10000)
(define kogel-snelheid 75)
(define alienvloot-snelheid 225)
(define schiet-delay 1500)
(define power-up-snelheid 150)
(define explosie-duur 100)

(define alienvloot-freeze-duur 10000)
(define raketscutum-duur 10000)
(define scoreverdubbelaar-duur 10000)
(define super-kogel-duur 5000)

(define aantal-powerups 6)
(define super-kogel 0)
(define alienvlootfreezer 1)
(define raketscutum 2)
(define levens-hersteller 3)
(define scoreverdubbelaar 4)
(define torpedo 5)

(define scorebord-breedte (quotient venster-breedte-px 5))
(define scorebord-hoogte (* cel-hoogte-px 2))
(define scorebord-lettergrootte (quotient cel-breedte-px 2))
(define kloof-scoretekst cel-hoogte-px)

(define harten-x-positie (- venster-breedte-px
                            cel-breedte-px))
(define afstand-tussen-harten cel-breedte-px)

(define score-gele-aliens 10)
(define score-paarse-aliens 20)
(define score-groene-aliens 30)


(define initiele-vlootbreedte 5)
(define initiele-vloothoogte 4)

(define initiele-raket-levens 5)

(define powerup-lettergrootte (quotient cel-breedte-px 2))

(define powerups-boodschappen-breedte
  (round (* venster-breedte-px (/ 5 8))))

(define powerups-boodschappen-hoogte cel-hoogte-px)

(define powerups-boodschappen-x-positie
  (quotient venster-breedte-px 4))

(define actieve-torpedos-breedte
  (* 4 cel-breedte-px))

(define actieve-torpedo-hoogte cel-hoogte-px)

(define actieve-torpedo-x-positie
  (- (quotient venster-breedte-px 2)
     cel-breedte-px))

(define actieve-torpedo-y-positie cel-hoogte-px)
(define actieve-torpedo-letter-grootte (quotient cel-breedte-px 2))

(define super-kogel-boodschappositie 0)

(define alienvlootfreezer-boodschappositie
  (quotient powerups-boodschappen-breedte 5))

(define raketscutum-boodschappositie
   (- (quotient powerups-boodschappen-breedte 2)
      cel-breedte-px))

(define scoreverdubbelaar-boodschappositie
  (* powerups-boodschappen-breedte
     (/ 2 3)))

(define huidig-level-nr-breedte (* 3 cel-breedte-px))
(define huidig-level-nr-hoogte cel-hoogte-px)

(define huidig-level-nr-x-positie
  (- venster-breedte-px (* cel-breedte-px 3)))
(define huidig-level-nr-y-positie cel-hoogte-px)

(define huidig-level-nr-lettergrootte (quotient cel-hoogte-px 2))

(define eindscherm-breedte (+ (* 4 cel-breedte-px)
                              (quotient venster-breedte-px 2)))
(define eindscherm-hoogte (quotient venster-hoogte-px 4))
(define eindscherm-x-positie (quotient venster-breedte-px 3))
(define game-over-lettergroootte (* cel-breedte-px 2))
(define druk-enter-lettergrootte cel-breedte-px)
(define druk-enter-y-positie (quotient eindscherm-hoogte 2))


(define kloof-tussen-aliens 2)
(define kloof-tussen-rijen 2)

(define gele-aliens-frequentie 0.20)
(define paarse-aliens-frequentie 0.70)


(define initieel-level 1)


(define initiele-levens-gele-aliens 1)
(define initiele-levens-paarse-aliens 2)
(define initiele-levens-groene-aliens 3)

(define DELETED 'deleted)

(define versnellingsterm-freq-schietende-kogels 3)
(define frequentie-powerup-gooiende-aliens 20)


(define geraakt #t)
