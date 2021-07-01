;------------- Powerup ADT --------------;
(define (maak-power-up obj-positie)
  ; ----> 0 = Super-Kogel        <------
  ; ----> 1 = Alienvlootfreezer  <------
  ; ----> 2 = Raketscutum        <------
  ; ----> 3 = Levens-hersteller  <------
  ; ----> 4 = Scoreverdubbelaar  <------
  ; ----> 5 = torpedo            <------
  (let ((positie (maak-positie (obj-positie 'get-x)
                               (obj-positie 'get-y)))
        (type (random aantal-powerups))
        (verwijder? #f)
        (power-up-tijd 0))
  
    (define (beweeg-omlaag!)
      ((positie 'beweeg!) 'omlaag))

    (define (verwijder!)
      (set! verwijder? #t))

    (define (set-tijd! nieuw)
      (set! power-up-tijd nieuw))

    (define (beweeg-power-up! delta-tijd)
      (define (buiten-het-scherm!)
        (let ((y-waarde (positie 'get-y)))
          (if (> y-waarde bodem)
              (verwijder!))))
      
      (set-tijd! (+ delta-tijd power-up-tijd))
      (if (> power-up-tijd power-up-snelheid)
          (begin
            (beweeg-omlaag!)
            (buiten-het-scherm!)
            (set-tijd! 0))))

    (define (dispatch-power-up msg)
      (cond ((eq? msg 'get-type) type)
            ((eq? msg 'positie) positie)
            ((eq? msg 'beweeg!) beweeg-power-up!)
            ((eq? msg 'verwijder?) verwijder?)
            ((eq? msg 'verwijder!) (verwijder!))
            ((eq? msg 'richting) 'omlaag)
            (else (error "onbekend bericht" msg))))
    dispatch-power-up))