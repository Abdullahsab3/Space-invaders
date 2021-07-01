;-------------------- Alienvloot ADT ----------------------;

(define (maak-alienvloot level)
  (let* ((vloot #f)
         (gegooide-powerups (make-headed-list 'power-ups))
         (aantal-aanwezige-aliens 0)
         (richting 'rechts)
         (kleur-laatst-geraakte-alien #f)
         (kleur-aliens-geraakt-door-torpedo (make-headed-list 'kleur-aliens))
         (schiet-tijd 0)
         (power-up-gooitijd 0)
         (vloot-door-kogel-geraakt? #f))

    ;; Wijzig de beweegrichting van de vloot naar de tegengestelde richting.
    (define (set-tegengestelde-richting!)
      
      (define (richting! nieuwe-richting)
        (set! richting nieuwe-richting))
      
      (cond ((eq? richting 'rechts) (richting! 'links))
            ((eq? richting 'links) (richting! 'rechts))))


    ;; de vloot laten bewegen in de meegegeven richting
    (define (beweeg!)
      (voor-alle-aliens
       (lambda (alien)
         ((alien 'beweeg!)
          richting))))
    
    ;; de vloot met 1 eenheid naar beneden laten bewegen
    (define (beweeg-eenheid-omlaag!)
      (voor-alle-aliens
       (lambda (alien)
         (alien 'eenheid-omlaag!))))

    (define (map-over-aliens f)
      (matrix-map vloot
                  (lambda (el)
                    (if (eq? el DELETED)
                        DELETED
                        (f el)))))
    
    ;; een procedue toepassen op alle aliens in de vloot
    (define (voor-alle-aliens f)
      (matrix-for-each vloot
                       (lambda (el)
                         (if (not (eq? el DELETED))
                             (f el)))))

    ;; vloot updaten als er iets aangepast wordt
    (define (update-vloot! arg)
      (set! vloot arg))

    ;; afstand tussen de aliens en afstand tussen de rijen
    (define (afstand-tussen-aliens idx)
      (* kloof-tussen-aliens idx))
    
    (define (afstand-tussen-rijen idx)
      (+ 2 (* kloof-tussen-rijen idx)))

    ;; hoe hoger de frequentie, hoe minder aliens powerups gooien
    (define (bepaal-powerup-gooiende-alien freq)
      (random-boolean freq))

    ;; hoe hoger de frequentie, hoe meer schietende aliens
    (define (bepaal-schietende-alien freq)
      (not (random-boolean (+ versnellingsterm-freq-schietende-kogels
                              freq))))
    ;; een alien-object aanmaken en toevoegen aan een rij
    (define (maak-alien-voor-rij alien-kleur rijnummer idx)
      (let* ((alien-positie (maak-positie (afstand-tussen-aliens idx)
                                          (afstand-tussen-rijen rijnummer)))
             (schietende-alien? (bepaal-schietende-alien level))
             (powerup-gooinede-alien? (bepaal-powerup-gooiende-alien
                                       frequentie-powerup-gooiende-aliens))
             (alien (maak-alien alien-positie
                                alien-kleur
                                schietende-alien?
                                powerup-gooinede-alien?)))
        alien))

    
    (define (maak-alienrij! aantal-aliens kleur rijnummer)
      (define alienrij (make-vector aantal-aliens '()))
      (let loop ((idx 0))
        (if (= idx aantal-aliens)
            alienrij
            (let ((alien (maak-alien-voor-rij kleur
                                              rijnummer
                                              idx)))
              (vector-set! alienrij idx alien)
              (loop (+ idx 1))))))

    ;; een willekeurige alienkleur kiezen voor de kleur van een hele rij
    (define (random-alienkleur)
      (let ((random-freq (random)))
        (cond ((< random-freq gele-aliens-frequentie) 'geel)
              ((> paarse-aliens-frequentie random-freq) 'paars)
              (else 'groen))))

    ;; aan alienvloot maken. Eventueel met een optioneel schema van hoe de vloot eruit kan zien
    (define (maak-alienvloot! hoogte breedte . schema)
      (set! aantal-aanwezige-aliens (* hoogte breedte))
      (let ((alienvloot (make-vector hoogte)))
        (let loop ((idx 0))
          (if (= idx hoogte)
              (update-vloot! alienvloot)
              (let* ((kleur (if (null? schema)
                                (random-alienkleur)
                                (let ((kl (car schema)))
                                       (set! schema (cdr schema))
                                  kl)))
                     (alienrij (maak-alienrij! breedte kleur idx)))
                (vector-set! alienvloot idx alienrij)
                (loop (+ idx 1)))))))

    (define (verwijder-alien-uit-de-vloot! alien)
      (remove-from-matrix! vloot alien))
    
    (define (voeg-toe-aan-power-ups! powerup)
      (add-to-headed-list! powerup gegooide-powerups))
    
    (define (decr-aantal!)
      (set! aantal-aanwezige-aliens
            (- aantal-aanwezige-aliens 1)))

    ;; als een alien door de kogel geraakt is verwijder hem uit de vloot
    (define (pas-aan-indien-geraakt!)
      (voor-alle-aliens (lambda (alien)
                          (if (alien 'verwijder?)
                              (let ((alien-positie (alien 'positie)))
                                (if (alien 'gooi-powerup?)
                                    (let ((powerup (maak-power-up alien-positie)))
                                      (voeg-toe-aan-power-ups! powerup)))
                                (verwijder-alien-uit-de-vloot! alien)
                                (decr-aantal!))
                              alien)))
      (set! vloot-door-kogel-geraakt? #f))

    
    
    ;; kijken of een alien geraakt is door een kogel.

    (define (geraakt? pos)
      (voor-alle-aliens
       (lambda (alien)
         (let ((geraakt?
                ((alien 'is-geraakt?) pos)))
           (if geraakt?
               (let ((alien-kleur (alien 'get-type)))
                 (set! vloot-door-kogel-geraakt? geraakt?)
                 (set! kleur-laatst-geraakte-alien alien-kleur)
                 (alien 'pas-aan-indien-geraakt!))))))
      vloot-door-kogel-geraakt?)

    (define (vind-alien-in-vloot alien)
             (find-in-matrix vloot alien eq?))

    ;; kijken of een alien geraakt is door een torpedo
    ;; en de nodige aanpassingen in de omliggende aliens brengen.

    (define (door-torpedo-geraakt? torpedo-pos)
      (let ((coordinaten-geraakte-alien #f))
        (voor-alle-aliens
         (lambda (alien)
           (let ((geraakt? ((alien 'is-geraakt?) torpedo-pos)))
             (if geraakt?
                 (begin (set! coordinaten-geraakte-alien
                              (vind-alien-in-vloot alien))
                        (alien 'pas-aan-indien-geraakt!)
                        (add-to-headed-list! (alien 'get-type)
                                             kleur-aliens-geraakt-door-torpedo)
                        (for-each-on-surrounding vloot
                                                 coordinaten-geraakte-alien
                                                 (lambda (alien)
                                                   (let ((alien-kleur (alien 'get-type)))
                                                     (add-to-headed-list! alien-kleur
                                                                kleur-aliens-geraakt-door-torpedo)
                                                   (alien 'pas-aan-indien-geraakt!)))))))))
        coordinaten-geraakte-alien))
        
                                       
    (define (bodem-geraakt?)
      (let ((alien-bodem-al-geraakt? #f))
        (voor-alle-aliens
         (lambda (alien)
           (if (alien 'bodem-geraakt?)
               (set! alien-bodem-al-geraakt? #t))))
        alien-bodem-al-geraakt?))
        

    (define (incr-vloothoogte getal)
      (+ getal initiele-vloothoogte))
    
    (define (incr-vlootbreedte getal)
      (+ getal initiele-vlootbreedte))

    (define (incr-level!)
      (set! level (+ level 1)))
      

    ;; het level van de alienvloot verhogen
    ;; --> een nieuwe alienvloot maken
    (define (level-up!)
      (incr-level!)
      (set! richting 'rechts)
      (apply maak-alienvloot!
             (append! (list  (incr-vloothoogte level)
                             (incr-vlootbreedte level))
                      (cond ((= level 2) '(groen
                                           groen
                                           paars
                                           paars
                                           geel
                                           geel))
                            ((= level 3) '(groen
                                           groen
                                           groen
                                           paars
                                           paars
                                           paars
                                           geel))
                            (else '())))))

    
    (define (set-schiettijd! nieuw)
      (set! schiet-tijd nieuw))

    ;; een willekeurig alien uit de vloot halen
    (define (random-alien)
      (random-element vloot))

    ;; een willekeurige alien uit de vloot halen en die laten schieten ..
    ;; indien die wel een schietende alien is.
    (define (schiet! delta-tijd)
      (set-schiettijd! (+ delta-tijd schiet-tijd))
      (if (> schiet-tijd schiet-delay)
          (let* ((rand-alien (random-alien))
                 (kogel (if (not (eq? rand-alien DELETED))
                            (rand-alien 'schiet!))))
            (set-schiettijd! 0)
            kogel)))
    
    (define (dispatch-alienvloot msg)
      (cond ((eq? msg 'beweeg!) (beweeg!))
            ((eq? msg 'bodem-geraakt?) (bodem-geraakt?))
            ((eq? msg 'beweeg-eenheid-omlaag!) (beweeg-eenheid-omlaag!))
            ((eq? msg 'set-tegengestelde-richting!) (set-tegengestelde-richting!))
            ((eq? msg 'map-over-aliens) map-over-aliens)
            ((eq? msg 'voor-alle-aliens) voor-alle-aliens)
            ((eq? msg 'aantal-aliens) aantal-aanwezige-aliens)
            ((eq? msg 'geraakt?) geraakt?)
            ((eq? msg 'pas-aan-indien-geraakt!) (pas-aan-indien-geraakt!))
            ((eq? msg 'kleur-laatst-geraakte-alien) kleur-laatst-geraakte-alien)
            ((eq? msg 'kleur-aliens-geraakt-door-torpedo) kleur-aliens-geraakt-door-torpedo)
            ((eq? msg 'level-up!) (level-up!))
            ((eq? msg 'schiet!) schiet!)
            ((eq? msg 'gegooide-powerups) gegooide-powerups)
            ((eq? msg 'door-torpedo-geraakt?) door-torpedo-geraakt?)
            (else (error "Onbekend bericht" msg))))

    ;; initele level
    (maak-alienvloot! initiele-vloothoogte initiele-vlootbreedte
                                    'groen
                                    'paars
                                    'geel
                                    'geel)

    
    dispatch-alienvloot))
