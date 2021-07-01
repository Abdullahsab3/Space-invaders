;--------------------- Alien ADT ----------------------;
(define (maak-alien pos kleur schietende-alien? power-up?)
  (let ((levenobj (maak-levens-ADT
                   ;; welke kleur de alien heeft. (Dit bepaalt ook het aantal levens)
                   (cond ((eq? kleur 'geel)
                          initiele-levens-gele-aliens)
                         ((eq? kleur 'paars)
                          initiele-levens-paarse-aliens)
                         ((eq? kleur 'groen)
                          initiele-levens-groene-aliens))))
        (botsingstoestand (maak-botsingstoestand pos)))

    (define (beweeg! richting)
      ((pos 'beweeg!) richting))

    (define (eenheid-omlaag!)
        ((pos 'beweeg!) 'omlaag))

    (define (is-geraakt? antipos)
      ((botsingstoestand  'is-geraakt?) antipos))

    ;; Wat er moet gebeuren indien een kogel een alien raakt
    (define (pas-aan-indien-geraakt!)
      ((botsingstoestand 'pas-aan-indien-geraakt!)
       (lambda ()
         (levenobj 'verminder-levens!))))

    ;; Deze procedure geeft aan wanneer de alien welke kant van het scherm raakt
    (define (zijkant-geraakt?)
      (let ((x-waarde (pos 'get-x)))
        (or (= x-waarde rechterzijkant)
            (= x-waarde linkerzijkant))))
    
    ;; Een predikaat om na te gaan of de aliens geen levens meer heeft.
    (define (nul-levens?)
      (levenobj 'nul-levens?))

    ;; Nagaan of de alien de bodem van het spel geraakt heeft.
    (define (bodem-geraakt?)
      (let ((y-waarde (pos 'get-y)))
        (= y-waarde bodem)))

    (define (schiet!)
      (if schietende-alien?
          (maak-kogel pos 'omlaag)))

    (define (dispatch-alien msg)
      (cond ((eq? msg 'positie) pos)
            ((eq? msg 'beweeg!) beweeg!)
            ((eq? msg 'eenheid-omlaag!) (eenheid-omlaag!))
            ((eq? msg 'get-type) kleur)
            ((eq? msg 'zijkant-geraakt?) (zijkant-geraakt?))
            ((eq? msg 'bodem-geraakt?) (bodem-geraakt?))
            ((eq? msg 'verwijder?) (nul-levens?))
            ((eq? msg 'is-geraakt?) is-geraakt?)
            ((eq? msg 'pas-aan-indien-geraakt!) (pas-aan-indien-geraakt!))
            ((eq? msg 'schiet!) (schiet!))
            ((eq? msg 'gooi-powerup?) power-up?)
            (else (error "onbekend bericht" msg))))
    dispatch-alien))