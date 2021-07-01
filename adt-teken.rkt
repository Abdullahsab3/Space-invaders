;--------------------------- Teken ADT -----------------------------------;
(define (maak-teken pixels-horizontaal pixels-verticaal)
  (let ((venster (make-window pixels-horizontaal pixels-verticaal
                              "Space Invaders" max-fps)))
    
    ((venster 'set-background!) "black")

    ;; teken-object! ((symbol -> any) tile -> /)
    ;; deze procedure zal de tile tekenen op het scherm op de positie van het object ..
    ;; .. dat meegegeven wordt bij het procedure-oproep.
    ;; Overgenomen van het voorbeeldspel Snake dat op Canvas beschikbaar is.
    ;; oplossingen\adt-teken.rkt vanaf codelijn 79 tot en met 85
    (define (teken-object! obj tile)
      (let* ((object-positie (obj 'positie))
             (object-x (object-positie 'get-x))
             (object-y (object-positie 'get-y))
             (screen-x (* object-x cel-breedte-px))
             (screen-y (* object-y cel-hoogte-px)))
        ((tile 'set-x!) screen-x)
        ((tile 'set-y!) screen-y)))

    ;; Procedures die gebruikt worden om objecten die in een lijst zitten te tekenen.

    ;; Deze procedure is verantwoordelijk voor het toevoegen van een associatie ..
    ;; .. bestaande uit het object in de car en zijn tile in de cdr aan de lijst van tiles.
    ;; ((symbol -> any) tile-object pair layer-object) -> tile)
    (define (voeg-object-toe! object tile tileslijst laag)
      (let* ((nieuwe-tile tile)
             (object-en-tile (cons object nieuwe-tile)))
        (add-to-headed-list! object-en-tile tileslijst)
        ((laag 'add-drawable) nieuwe-tile)
        nieuwe-tile))

    (define get-object car)
    (define get-tile cdr)
    ;; Deze procedure zal een tile van een object zoeken in een tileslijst ..
    ;; aan de hand van dat object. Indien het object niet gevonden is, dan wordt het aan de tileslijst ..
    ;; .. toegevoegd m.b.v. voeg-object-toe!.
    ;; ((symbol -> any) pair ((symbol -> any) tile-object pair layer-object) -> tile) -> tile)
    (define (neem-tile object tileslijst voeg-object-toe!)
      (let ((resultaat (assoc object (content tileslijst))))
        (if resultaat
            (get-tile resultaat)
            (voeg-object-toe! object))))

    ;; Deze procedure zal de tile van een object uit de tileslijst halen en het object tekenen op het scherm.
    (define (teken-object-tiles-lijst! neem-tile object)
      (let ((tile (neem-tile object)))
        (teken-object! object tile)))
    
    ;; Deze procedure neemt een lijst van objecten (van hetzelfde objectà binnen en tekent ze vervolgens op het scherm.
    (define (teken-objecten! teken-object! objecten)
      (for-each-in-headed-list (lambda (object)
                                 (teken-object! object)) objecten))

    ;; Deze procedure zal de objecten uit het scherm verwijderen wanneer het object in de spellogica aangeeft dat hij verwijderd moet worden.
    (define (verwijder-objecten! laag tileslijst)
      (for-each-in-headed-list
       (lambda (assocls)
         (let ((object (get-object assocls))
               (tile (get-tile assocls)))
           (if (object 'verwijder?)
               (begin ((laag 'remove-drawable) tile)
                      (remove-from-headed-list! assocls tileslijst)))))
       tileslijst))

   
    (define (teken/verwijder-objecten! objecten tileslijst laag teken-object!)
      (teken-objecten! teken-object! objecten)
      (verwijder-objecten! laag tileslijst))

    ;; Deze procedure zal alle objecten uit het scherm verwijderen (handig wanneer het spel gedaan is).
    (define (verwijder-alle-objecten! tileslijst laag)
      (for-each-in-headed-list (lambda (assocls)
                                 (let ((object (car assocls))
                                       (tile (cdr assocls)))
                                   ((laag 'remove-drawable) tile)
                                   (remove-from-headed-list! assocls tileslijst)))
                               tileslijst))

    ;-----------------------  Een explosie tekenen ------------------------;

    (define explosies-laag (venster 'make-layer))
    (define explosies-tiles (make-headed-list 'explosies-tiles))

    (define explosies (make-headed-list 'explosies))

    (define (voeg-toe-aan-explosies! explosie)
      (add-to-headed-list! explosie explosies))


    (define (voeg-explosie-toe! explosie-adt)
      (voeg-object-toe! explosie-adt (make-bitmap-tile "images/explosie..png")
                        explosies-tiles explosies-laag))
      
    
    (define (neem-explosie explosie-adt)
      (neem-tile explosie-adt explosies-tiles voeg-explosie-toe!))

    (define (teken-explosie! explosie-adt)
      (let ((tile (neem-explosie explosie-adt)))
        (teken-object! explosie-adt tile)))

    (define (teken/verwijder-explosies! explosies)
      (teken/verwijder-objecten! explosies explosies-tiles
                                 explosies-laag teken-explosie!))

    (define (verwijder-alle-explosies!)
      (verwijder-alle-objecten! explosies-tiles explosies-laag))

    ;------------------- Alienvloot tekenen en verwijderen ------------------------;
    ;; Onderstaande methode om de alienvloot te tekenen is gebaseerd op de methode ..
    ;; .. van de slangstukken tekenen in het voorbeeldspel Snake dat te vinden is op Canvas.
    
    ;; alienvloot-laag aanmaken
    (define alienvloot-laag (venster 'make-layer))
    ;; lijst van alien tiles
    (define alienvloot-tiles (make-headed-list 'alienvloot-tiles))

    ;; een alien-tile aanmaken
    (define (alien-tile alien-adt)
      ;; In latere fases kan het zijn dat er andere soorten aliens toegevoegd worden
      ;; daarom wordt hier een string-append gedaan.
      ;; + codeduplicatie vermijden.
      (let* ((type (symbol->string (alien-adt 'get-type)))
             (prefix (string-append "images/alien-" type)))
        (make-bitmap-tile (string-append prefix ".png"))))
    
    ;; een alien toevoegen aan de alienvloot tiles en laag.
    ;;; alien-adt -> tile
    (define (voeg-alienschip-toe! alien-adt)
      (voeg-object-toe! alien-adt (alien-tile alien-adt)
                        alienvloot-tiles alienvloot-laag))
    
    ;; een alien uit de alienvloot tiles halen of toevoegen ..
    ;; .. indien die nog niet toegevoegd was.
    (define (neem-alienschip alien-adt)
      (neem-tile alien-adt alienvloot-tiles voeg-alienschip-toe!))

    (define (teken-alien! alien-adt)
      (teken-object-tiles-lijst! neem-alienschip alien-adt))

    ;; alle aliens in de alienvloot op het scherm tekenen of verwijderen.
    ;; (Hier had ik mijn abstractie een beetje moeten breken aangezien dat aliens tekenen een paar extra zaken vereist :( ,
    ;; zoals explosies tekenen wanneer de aliens dood zijn. Bovendien zitten de aliens in een vector.)
    (define (verwijder-aliens!)
      (for-each-in-headed-list (lambda (assocls)
                                 (let ((alien (car assocls))
                                       (tile (cdr assocls)))
                                   (if (alien 'verwijder?)
                                       (let* ((pos (alien 'positie))
                                              (explosie (maak-explosie pos)))
                                         (voeg-toe-aan-explosies! explosie)
                                         ((alienvloot-laag 'remove-drawable) tile)
                                         (remove-from-headed-list! assocls alienvloot-tiles)))))
                               alienvloot-tiles))
 
    (define (teken/verwijder-alienvloot! alienvloot-adt)
      ((alienvloot-adt 'voor-alle-aliens) (lambda (alien)
                                            (teken-alien! alien)))
      (verwijder-aliens!))

    (define (verwijder-alle-aliens!)
      (verwijder-alle-objecten! alienvloot-tiles alienvloot-laag))

    

      
    ;------------------- Kogels tekenen en verwijderen ------------------------;
    
    (define kogels-laag (venster 'make-layer))
    (define kogels-tiles (make-headed-list 'kogels-tiles))

    (define (voeg-kogel-toe! kogel-adt)
      (voeg-object-toe! kogel-adt
                        (make-bitmap-tile "images/kogel.png"
                                          "images/kogel-mask.png")
                        kogels-tiles kogels-laag))

    (define (neem-kogel kogel-adt)
      (neem-tile kogel-adt kogels-tiles voeg-kogel-toe!))

    (define (teken-kogel! kogel-adt)
      (teken-object-tiles-lijst! neem-kogel kogel-adt))

    (define (teken/verwijder-kogels! kogel-adts)
      (teken/verwijder-objecten! kogel-adts kogels-tiles
                                 kogels-laag teken-kogel!))

    (define (verwijder-alle-kogels!)
      (verwijder-alle-objecten! kogels-tiles kogels-laag))

    ;------------------- Torpedos tekenen en verwijderen -------------------;
    (define torpedo-laag (venster 'make-layer))
    (define torpedo-tiles (make-headed-list 'torpedo-tiles))


    (define (voeg-torpedo-toe! torpedo)
      (voeg-object-toe! torpedo
                        (make-bitmap-tile "images/torpedo.png"
                                          "images/torpedo-mask.png")
                        torpedo-tiles torpedo-laag))

    (define (neem-torpedo torpedo)
      (neem-tile torpedo torpedo-tiles voeg-torpedo-toe!))

    (define (teken-torpedo! torpedo)
      (teken-object-tiles-lijst! neem-torpedo torpedo))

    (define (teken/verwijder-torpedos! torpedos)
      (teken/verwijder-objecten! torpedos torpedo-tiles
                                 torpedo-laag teken-torpedo!))

    (define (verwijder-alle-torpedos!)
      (verwijder-alle-objecten! torpedo-tiles torpedo-laag))

      
    
    ;-------------------------- De raket tekenen ----------------------------;
    
    (define raket-laag (venster 'make-layer))
    (define raket-tile
      (make-bitmap-tile "images/raket.png"
                        "images/raket-mask.png"))
    ((raket-laag 'add-drawable) raket-tile)

    (define (teken-raket! raket-adt)
      (teken-object! raket-adt raket-tile))

    (define (verwijder-raket!)
      ((raket-laag 'remove-drawable) raket-tile))

    ;------------------ Het scorebord laten tekenen ---------------------;
    (define scorebord (venster 'make-layer))
    (define scorebord-tile
      (make-tile scorebord-breedte
                 scorebord-hoogte))
    ((scorebord 'add-drawable) scorebord-tile)

    (define (teken-scorebord! scorebord-adt)
      (let ((huidige-score (number->string (scorebord-adt 'huidige-score)))
            (hoogste-score (number->string (scorebord-adt 'hoogste-score))))
        (scorebord-tile 'clear)
        ((scorebord-tile 'draw-text)
         (string-append "Huidige score: " huidige-score)
         scorebord-lettergrootte 0 0  "white")
        ((scorebord-tile 'draw-text)
         (string-append "Hoogste score: " hoogste-score)
         scorebord-lettergrootte 0 kloof-scoretekst "white")))

    (define (verwijder-scorebord!)
      (scorebord-tile 'clear))

    ;----------------- Het raket levenobject laten tekenen --------------;
    (define levens-laag (venster 'make-layer))
    (define levens-tiles (make-vector initiele-raket-levens))
    ;; hoeveel harten momenteel op het scherm getekend zijn.
    (define hart-teller 0)

    (define (incr-hart-teller!)
      (set! hart-teller (+ hart-teller 1)))

    (define (decr-hart-teller!)
      (set! hart-teller (- hart-teller 1)))
    
    (define (voeg-hart-toe! idx)
      (let ((nieuwe-tile
             (make-bitmap-tile "images/hart.png")))
        (vector-set! levens-tiles idx nieuwe-tile)
        ((levens-laag 'add-drawable) nieuwe-tile)
        ;; Aangezien de harten langs links van het venster getekend worden, wordt telkens ..
        ;; .. de harten-x-positie afgetrokken van de afstand tussen de harten ..
        ;; .. vermenigvuldigd met de index van het hart.
        ((nieuwe-tile 'set-x!) (- harten-x-positie
                                  (* idx afstand-tussen-harten)))
        (incr-hart-teller!)
        nieuwe-tile))

    
    (define (fill-laag!)
      (define (loop)
        ;; als de hart-teller (het aantal getekende harten op het scherm) niet gelijk is ..
        ;; .. aan de initiele raketlevens, dan worden meer harten op het scherm getekend tot dat het zo is.
        (if (not (= hart-teller
                    initiele-raket-levens))
            (begin (voeg-hart-toe! hart-teller)
                   (loop))))
      (loop))
 
    (fill-laag!)
    
    (define (verwijder-hart-uit-vector!)
      (vector-set! levens-tiles (- hart-teller 1) DELETED)
      (decr-hart-teller!))

    (define (neem-hart idx)
      (vector-ref levens-tiles (- idx 1)))
         

    (define (verwijder/herstel-hart! levens-adt)
      (let ((huidig-aantal-levens (levens-adt 'get-levens)))
        (cond ((< huidig-aantal-levens hart-teller)
               ((levens-laag 'remove-drawable) (neem-hart hart-teller))
               (verwijder-hart-uit-vector!))
              ;; voor levens-hersteller-powerup
              ((and (= huidig-aantal-levens initiele-raket-levens)
                    (< hart-teller initiele-raket-levens))
               (fill-laag!)))))

    (define (verwijder-alle-harten!)
      (vector-for-each (lambda (tile)
                         (if (not (eq? tile DELETED))
                             ((levens-laag 'remove-drawable) tile)))
                       levens-tiles)
      (set! hart-teller 0))

    ;----------------- De power up bolletjes laten tekenen -------------;

    (define power-ups-laag (venster 'make-layer))
    (define power-ups-tiles (make-headed-list 'power-ups-tiles))

    (define (power-up-tile power-up-adt)
      (let* ((type (number->string (power-up-adt 'get-type)))
             (pad (string-append "images/powerup-" type ".png")))
        (make-bitmap-tile pad)))
    
    (define (voeg-power-up-toe! power-up-adt)
      (voeg-object-toe! power-up-adt
                        (power-up-tile power-up-adt)
                        power-ups-tiles power-ups-laag))


    (define (neem-power-up power-up-adt)
      (neem-tile power-up-adt
                   power-ups-tiles
                   voeg-power-up-toe!))

    (define (teken-power-up! power-up-adt)
      (teken-object-tiles-lijst! neem-power-up power-up-adt))

    (define (teken/verwijder-power-ups! power-ups)
      (teken/verwijder-objecten! power-ups power-ups-tiles
                                 power-ups-laag teken-power-up!))

    (define (verwijder-alle-powerups!)
      (verwijder-alle-objecten! power-ups-tiles power-ups-laag))
    
    ;--------------- actieve powerups laten tekenen -------------------;
    ;; een associatielijst die de boodschap en de positie van een tijdgebaseerde actieve powerup bijhoudt.
    (define powerups-boodschap-en-positie
      (list (list super-kogel
                  super-kogel-boodschappositie
                  "Super-Kogel")
            (list alienvlootfreezer
                  alienvlootfreezer-boodschappositie
                  "Alienvlootfreezer")
            (list raketscutum
                  raketscutum-boodschappositie
                  "Raketscutum")
            (list scoreverdubbelaar
                  scoreverdubbelaar-boodschappositie
                  "Scoreverdubbelaar")))

    (define actieve-powerups-laag (venster 'make-layer))
    (define actieve-powerups-tile
      (make-tile powerups-boodschappen-breedte
                 powerups-boodschappen-hoogte))
    ((actieve-powerups-laag 'add-drawable) actieve-powerups-tile)
    ((actieve-powerups-tile 'set-x!) powerups-boodschappen-x-positie)
    
    (define actieve-torpedos-tile
      (make-tile actieve-torpedos-breedte
                 actieve-torpedo-hoogte))
    
    ((actieve-powerups-laag 'add-drawable) actieve-torpedos-tile)
    ((actieve-torpedos-tile 'set-x!) actieve-torpedo-x-positie)
    ((actieve-torpedos-tile 'set-y!) actieve-torpedo-y-positie)

    (define (teken-torpedo-boodschap! aantal-beschikbare-torpedos)
      (actieve-torpedos-tile 'clear)
      (if (not (zero? aantal-beschikbare-torpedos))
          ((actieve-torpedos-tile 'draw-text)
           (string-append "torpedos: "
                          (number->string aantal-beschikbare-torpedos))
           actieve-torpedo-letter-grootte
           0
           0
           "white")))

    (define (teken-powerup-boodschap! boodschap poweruppositie)
      ((actieve-powerups-tile 'draw-text) boodschap
                                          powerup-lettergrootte
                                          poweruppositie
                                          0
                                          "white"))

    (define get-conscel-pos-boodschap cdr)
    (define get-positie car)
    (define get-boodschap cadr)
    
    (define (kies-powerup-boodschap type)
      (let ((res (assoc type powerups-boodschap-en-positie)))
        (if res
            (get-conscel-pos-boodschap res))))

    (define (teken-powerup-status! actieve-powerups niet-tijdgebaseerde-powerup?)
      (actieve-powerups-tile 'clear)
      (for-each-in-headed-list
       (lambda (actieve-powerup)
         (let ((welke-powerup (actieve-powerup 'get-type)))
           (if (not (niet-tijdgebaseerde-powerup? welke-powerup))
               (let* ((boodschap-en-positie (kies-powerup-boodschap welke-powerup))
                      (positie (get-positie boodschap-en-positie))
                      (boodschap (get-boodschap boodschap-en-positie)))
                 (teken-powerup-boodschap! boodschap positie)))))
       actieve-powerups))
      
    (define (verwijder-actieve-powerups!)
      ((actieve-powerups-laag 'remove-drawable) actieve-powerups-tile))

    (define (verwijder-aantal-torpedos!)
      ((actieve-powerups-laag 'remove-drawable) actieve-torpedos-tile))
      
    

    ;------------- huidige level (nummer) laten tekenen ----------------;
    
    (define huidig-level-nr-laag (venster 'make-layer))
    (define huidig-level-nr-tile (make-tile huidig-level-nr-breedte
                                         huidig-level-nr-hoogte))
    ((huidig-level-nr-laag 'add-drawable) huidig-level-nr-tile)
    ((huidig-level-nr-tile 'set-x!) huidig-level-nr-x-positie)
    ((huidig-level-nr-tile 'set-y!) huidig-level-nr-y-positie)

    (define (teken-huidig-level-nr! huidige-level-nr)
      (let* ((level-string (number->string huidige-level-nr))
             (tot (string-append "level: " level-string)))
        (huidig-level-nr-tile 'clear)
        ((huidig-level-nr-tile 'draw-text) tot
                                           huidig-level-nr-lettergrootte
                                           0
                                           0
                                           "white")))
  
    (define (verwijder-huidig-level-nr!)
      (huidig-level-nr-tile 'clear))
 
    ;------------------- Het spel laten tekenen ------------------------;
    
    (define (teken-spel! spel-adt)
      (teken-level! (spel-adt 'level)))
    
    (define (teken-level! level-adt)
      (teken-huidig-level-nr! (level-adt 'huidig-level))
      (teken-scorebord! (level-adt 'scorebord))
      (verwijder/herstel-hart! (level-adt 'raket-levens))
      (teken-raket! (level-adt 'raket))
      (teken/verwijder-kogels! (level-adt 'kogels))
      (teken/verwijder-torpedos! (level-adt 'torpedos))
      (teken/verwijder-alienvloot! (level-adt 'alienvloot))
      (teken/verwijder-power-ups! (level-adt 'power-ups))
      (teken-powerup-status! (level-adt 'actieve-powerups)
                             (level-adt 'niet-tijdgebaseerde-powerup?))
      (teken-torpedo-boodschap! (level-adt 'aantal-beschikbare-torpedos))
      (teken/verwijder-explosies! explosies))

    (define (update-in-teken! delta-tijd)
      (for-each-in-headed-list (lambda (explosie)
                                 (if (explosie 'verwijder?)
                                     (remove-from-headed-list! explosie explosies)
                                     ((explosie 'toon-explosie!) delta-tijd)))
                               explosies))

    ;----------------------- Het eindscherm tekenen ------------------------;
    
    (define eindscherm (venster 'make-layer))
    (define eindscherm-tile
      (make-tile eindscherm-breedte
                 eindscherm-hoogte))
    ((eindscherm 'add-drawable) eindscherm-tile)

    ((eindscherm-tile 'set-y!) eindscherm-x-positie)


    (define (teken-gameover-boodschap!)
      ((eindscherm-tile 'draw-text)  "Game Over!"
                                     game-over-lettergroootte
                                     0
                                     0
                                     "white"))
    
    (define (teken-druk-enter-boodschap!)
      ((eindscherm-tile 'draw-text) "Druk op Enter om het spel te herstarten."
                                    druk-enter-lettergrootte
                                    0
                                    druk-enter-y-positie
                                    "white"))
      
    (define (teken-gameover!)
      (eindscherm-tile 'clear)
      (verwijder-alles!)
      (teken-gameover-boodschap!)
      (teken-druk-enter-boodschap!))
 

    ;; alles uit het scherm verwijderen als het spel gedaan is.
    (define (verwijder-alles!)
      (verwijder-alle-powerups!)
      (verwijder-actieve-powerups!)
      (verwijder-aantal-torpedos!)
      (verwijder-huidig-level-nr!)
      (verwijder-raket!)
      (verwijder-alle-harten!)
      (verwijder-alle-kogels!)
      (verwijder-alle-aliens!)
      (verwijder-alle-explosies!)
      (verwijder-alle-torpedos!))

    (define (herteken!)
      (eindscherm-tile 'clear)
      ((raket-laag 'add-drawable) raket-tile)
      ((actieve-powerups-laag 'add-drawable) actieve-powerups-tile)
      ((actieve-powerups-laag 'add-drawable) actieve-torpedos-tile))
    
    (define (set-spel-lus-functie! update-in-spel!)
      ((venster 'set-update-callback!) (lambda (delta-tijd)
                                         (update-in-spel! delta-tijd)
                                         (update-in-teken! delta-tijd))))
    
    (define (set-toets-functie! fun)
      ((venster 'set-key-callback!) fun))


    (define (dispatch-teken-adt msg)
      (cond ((eq? msg 'set-toets-functie!) set-toets-functie!)
            ((eq? msg 'set-spel-lus-functie!) set-spel-lus-functie!)
            ((eq? msg 'teken-gameover!) (teken-gameover!))
            ((eq? msg 'verwijder-alle-aliens!) (verwijder-alle-aliens!))
            ((eq? msg 'teken-spel!) teken-spel!)
            ((eq? msg 'herteken!) (herteken!))
            (else (error "onbekend bericht" msg))))
    
    dispatch-teken-adt))
