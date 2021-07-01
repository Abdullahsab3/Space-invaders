;----------------- Het Level ADT -------------------;
(define (maak-adt-level huidig-level aantal-cellen-breedte aantal-cellen-hoogte)
  (let* ((raket-start-positie
          (maak-positie (half aantal-cellen-breedte)
                        (- aantal-cellen-hoogte 1)))
         (raket-adt (maak-raket raket-start-positie))
         (alienvloot-adt (maak-alienvloot huidig-level))
         (alienvloot-tijd 0)
         (kogels (make-headed-list 'kogels-adts))
         (power-ups (alienvloot-adt 'gegooide-powerups))
         (actieve-powerups (make-headed-list 'actieve-powerups))
         (actieve-powerups-adts #f)
         (scorebord (maak-scorebord-adt))
         (aantal-beschikbare-torpedos 0)
         (torpedos (make-headed-list 'torpedos))
         (level-gewonnen? #f))
    
      
    ;; raket besturen
    (define (bestuur-raket! toets)
      (cond ((eq? toets #\space)
             (voeg-toe-aan-kogels! (raket-adt 'schiet!)))
            ((eq? toets 'shift)
             (voeg-toe-aan-torpedos!
              (maak-torpedo (raket-adt 'positie)
                          'omhoog)))
            (else ((raket-adt 'beweeg-horizontaal!) toets))))

    
    ;; kogel afvuren (kan eventueel geabstraheerd worden want aliens moeten later kogels afvuren)
    (define (voeg-toe-aan-kogels! kogel)
      (add-to-headed-list! kogel kogels))


    ;; een procedure om, gegeven een object dat geschoten is, zoals een kogel of en powerup, ..
    ;; .. na te gaan of het object met een ander object botst
    (define (botsing? geschoten-obj)
      (let* ((geschoten-obj-pos (geschoten-obj 'positie))
             (geschoten-obj-richting (geschoten-obj 'richting))
             (obj (if (eq? geschoten-obj-richting 'omhoog) alienvloot-adt raket-adt))
             (obj-geraakt? ((obj 'geraakt?) geschoten-obj-pos)))
        (cons obj-geraakt? obj)))

    
    (define botsingstatus car)
    ;; anti-obj: het object waartegen de kogel/powerup gebotst is.
    (define anti-obj cdr)

    (define (object-botsing! obj doe-bij-botsing!)
      (let* ((botsing-detectie (botsing? obj))
             (botsing-gebeurd? (botsingstatus botsing-detectie))
             (antiobj (anti-obj botsing-detectie)))
        (if botsing-gebeurd?
            (doe-bij-botsing! obj antiobj))))


    (define (super-kogel-actief?)
      (find-in-headed-list actieve-powerups super-kogel
                           (lambda (actieve-powerup type)
                             (let ((actieve-type (actieve-powerup 'get-type)))
                               (eq? actieve-type type)))))

    (define (raketscutum-actief? obj)
      (and (eq? obj raket-adt)
           (find-in-headed-list actieve-powerups raketscutum
                                (lambda (actieve-powerup type)
                                  (let ((actieve-type (actieve-powerup 'get-type)))
                                    (eq? actieve-type type))))))

    (define (actie-bij-kogelbotsing! kogel antiobj)
      (if (not (raketscutum-actief? antiobj))
          (antiobj 'pas-aan-indien-geraakt!))

      (if (eq? antiobj alienvloot-adt)
          (let ((kleur (alienvloot-adt 'kleur-laatst-geraakte-alien)))
            ((scorebord 'pas-huidige-score-aan!) kleur)))
                         
      (if (super-kogel-actief?)
          (if (eq? antiobj raket-adt)
              (kogel 'verwijder!))
          (kogel 'verwijder!)))

    (define (kogel-botsing! kogel)
      (object-botsing! kogel actie-bij-kogelbotsing!))

    ;----------- Torpedos (actieve powerup) ----------;

    (define (torpedo-botsing? torpedo)
      (let* ((torpedo-positie (torpedo 'positie))
             (vloot-geraakt? ((alienvloot-adt 'door-torpedo-geraakt?)
                              torpedo-positie)))
        vloot-geraakt?))

    (define (update-score! hlst)
      (let loop ((kleuren (content hlst)))
        (if  (null? kleuren)
             (set-cdr! hlst '())
             (let ((eerste-kleur (car kleuren)))
               ((scorebord 'pas-huidige-score-aan!) eerste-kleur)
               (loop (cdr kleuren))))))

    (define (torpedo-botsing! torpedo)
      (let ((botsing-gebeurd? (torpedo-botsing? torpedo)))
        (if botsing-gebeurd?
            (begin (torpedo 'verwijder!)
                   (alienvloot-adt 'pas-aan-indien-geraakt!)
                   (let ((kleuren
                          (alienvloot-adt 'kleur-aliens-geraakt-door-torpedo)))
                     (update-score! kleuren))))))

    (define (incr-aantal-beschikbare-torpedos!)
      (set! aantal-beschikbare-torpedos
            (+ aantal-beschikbare-torpedos 1)))
    
    (define (decr-aantal-beschikbare-torpedos!)
      (set! aantal-beschikbare-torpedos
            (- aantal-beschikbare-torpedos 1)))

    (define maak-torpedo maak-kogel)
    
      (define (voeg-toe-aan-torpedos! nieuwe-torpedo)
        (if (not (zero? aantal-beschikbare-torpedos))
            (begin (add-to-headed-list! nieuwe-torpedo torpedos)
                   (decr-aantal-beschikbare-torpedos!))))

    (define (beweeg-torpedos! delta-tijd)
      (beweeg-objecten! torpedos
                        delta-tijd
                        torpedo-botsing!))
            

    ;-------- powerupbolletjes botsingslogica -------------;
    (define (geef-actieve-powerup-terug type)
      ((actieve-powerups-adts 'geef-actieve-powerup-terug) type))
    
    (define (voeg-toe-aan-actieve-powerups! actieve-powerup)
      (add-to-headed-list! actieve-powerup actieve-powerups))

    (define (activeer-powerup! type)
      (let ((actieve-powerup ((geef-actieve-powerup-terug type))))
        (voeg-toe-aan-actieve-powerups! actieve-powerup)))

    (define (actie-bij-powerupbotsing! powerup antiobj)
      (activeer-powerup! (powerup 'get-type))
      (powerup 'verwijder!))
      
    (define (powerup-botsing! powerup)
      (object-botsing! powerup actie-bij-powerupbotsing!))
      

    ;; Alle afgevuurde kogels laten bewegen.
    (define (beweeg-objecten! objecten delta-tijd botsing-actie!)
      (if (not (null? (content objecten)))
          (for-each (lambda (object) (if (object 'verwijder?)
                                         (remove-from-headed-list! object objecten)
                                         (begin ((object 'beweeg!) delta-tijd)
                                                (botsing-actie! object))))
                    (content objecten))))
                              
    (define (beweeg-kogels! delta-tijd)
      (beweeg-objecten! kogels delta-tijd kogel-botsing!))

    (define (beweeg-power-ups! delta-tijd)
      (beweeg-objecten! power-ups delta-tijd powerup-botsing!))

    
    ;; huidig aantal aanwezige aliens in de vloot weergeven
    (define (huidig-aantal-aliens)
      (alienvloot-adt 'aantal-aliens))

    (define (is-level-gewonnen?)
      (if (zero? (huidig-aantal-aliens))
          (set! level-gewonnen? #t))
      level-gewonnen?)

    ;; Als een alien een bodem geraakt heeft, dan heeft de speler verloren.
    (define (spel-verloren?)
    (or (raket-dood?)
           (alienvloot-adt 'bodem-geraakt?)))
    

    (define (initializeer-nieuw-level!)
      (set! huidig-level (+ huidig-level 1))
      (alienvloot-adt 'level-up!))

    (define (level-up!)
      (initializeer-nieuw-level!)
      (set! level-gewonnen? #f))


    (define (set-alienvloot-tijd! nieuwe-alienvloot-tijd)
      (set! alienvloot-tijd nieuwe-alienvloot-tijd))

    (define (versnel-alienvloot! versnellingsgraad)
      (- alienvloot-snelheid (* versnellingsgraad 10)))
 
    ;; alienvloot bewegingsmanoeuvre automatiseren
    (define (beweeg-alienvloot! delta-tijd)
      (set-alienvloot-tijd! (+ alienvloot-tijd delta-tijd))
      (if (> alienvloot-tijd (versnel-alienvloot! huidig-level))
          (begin (alienvloot-adt 'beweeg!)
                 (let ((zijkant-geraakt? #f))
                   ((alienvloot-adt 'voor-alle-aliens)
                    (lambda (alien) (if (alien 'zijkant-geraakt?)
                                        (set! zijkant-geraakt? #t))))
                   (if zijkant-geraakt?
                       (begin (alienvloot-adt 'beweeg-eenheid-omlaag!)
                              (alienvloot-adt 'set-tegengestelde-richting!))))
                 (set-alienvloot-tijd! 0))))

    (define (raket-dood?)
      (let* ((raket-levens (raket-adt 'get-levensobject)))
        (raket-levens 'nul-levens?)))

    (define (niet-tijdgebaseerde-powerup? type)
      ((actieve-powerups-adts 'niet-tijdgebaseerde-powerup?) type))


    ;; alle actieve powerups in gang zetten
    ;; en indien ze klaar zijn, uit de lijst verwijderen.
    (define (activeer-powerups! delta-tijd)
      (for-each-in-headed-list
       (lambda (actieve-powerup)
         (cond ((actieve-powerup 'verwijder?)
                (remove-from-headed-list! actieve-powerup actieve-powerups))
               ((niet-tijdgebaseerde-powerup? (actieve-powerup 'get-type))
                ((actieve-powerup 'voer-actie-uit!)))
               (else ((actieve-powerup 'voer-actie-uit!) delta-tijd))))
       actieve-powerups))



    ;; Dit is nodig voor het geval dat het geen alien geschoten wordt, ..
    ;; dan wordt er void teruggegeven.
    (define kogel? procedure?)
    
    
    ;; update delta-tijd
    (define (update! delta-tijd)
      (beweeg-kogels! delta-tijd)
      (beweeg-power-ups! delta-tijd)
      (beweeg-alienvloot! delta-tijd)
      (activeer-powerups! delta-tijd)
      (beweeg-torpedos! delta-tijd)
      (let ((potentiele-kogel ((alienvloot-adt 'schiet!) delta-tijd)))
        ;; als de potentiele kogel effectief een kogel is, dan voegen we hem toe
        ;; anders doen we niets
        (if (kogel? potentiele-kogel)
            (voeg-toe-aan-kogels! potentiele-kogel))))

    ;; toetsenbordinvoer aan spel geven.
    (define (toets! toets)
      (cond
        ;:; tests en snelle shortcuts
        ;; cheatknoppen om verschillende acties te doen
        ;; zoals naar het volgende level gaan en de powerups activeren
        ((eq? toets #\x) (set! level-gewonnen? #t))
        ((or (eq? toets 'numpad1)
             (eq? toets 'f1)) (activeer-powerup! super-kogel)) 
        ((or (eq? toets 'numpad2)
             (eq? toets 'f2))
         (activeer-powerup! alienvlootfreezer))
        ((or (eq? toets 'numpad3)
             (eq? toets 'f3))
         (activeer-powerup! raketscutum))
        ((or (eq? toets 'numpad4)
             (eq? toets 'f4))
         (activeer-powerup! levens-hersteller))
        ((or (eq? toets 'numpad5)
             (eq? toets 'f5))
         (activeer-powerup! scoreverdubbelaar))
        ((or (eq? toets 'numpad6)
             (eq? toets 'f6))
         (activeer-powerup! torpedo))
        ;; de raket besturen
        (else (bestuur-raket! toets))))

    (define (dispatch-level msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'toets!) toets!)
            ((eq? msg 'raket) raket-adt)
            ((eq? msg 'raket-levens) (raket-adt 'get-levensobject))
            ((eq? msg 'alienvloot) alienvloot-adt)
            ((eq? msg 'level-gewonnen?) (is-level-gewonnen?))
            ((eq? msg 'spel-verloren?) (spel-verloren?))
            ((eq? msg 'kogels) kogels)
            ((eq? msg 'torpedos) torpedos)
            ((eq? msg 'power-ups) power-ups)
            ((eq? msg 'level-up!) (level-up!))
            ((eq? msg 'scorebord) scorebord)
            ((eq? msg 'toets!) toets!)
            ((eq? msg 'huidig-level) huidig-level)
            ((eq? msg 'actieve-powerups) actieve-powerups)
            ((eq? msg 'niet-tijdgebaseerde-powerup?) niet-tijdgebaseerde-powerup?)
            ((eq? msg 'incr-aantal-beschikbare-torpedos!) incr-aantal-beschikbare-torpedos!)
            ((eq? msg 'aantal-beschikbare-torpedos) aantal-beschikbare-torpedos)
            (else (error "onbekend bericht" msg))))
    
    (set! actieve-powerups-adts (maak-actieve-powerups dispatch-level))
    
    dispatch-level))