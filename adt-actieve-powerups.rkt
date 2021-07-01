;------- Actieve powerups ADT --------;
(define (maak-actieve-powerups level-adt)
  (let ((actieve-powerups-adts
         ;; De lambdas zorgen ervoor dat de body van de lambda pas geevalueerd wordt ..
         ;;.. wanneer de lambda opgeroepen is.
         ;; Op die manier hoef ik niet telkens voor elke powerup een speciaal argument te geven.
         (vector  (lambda () (maak-actieve-super-kogel))
                  (lambda () (maak-actieve-alienvlootfreezer))
                  (lambda () (maak-actieve-raketscutum))
                  (lambda () (maak-actieve-raket-levenshersteller (level-adt 'raket-levens)))
                  (lambda () (maak-actieve-scoreverdubbelaar (level-adt 'scorebord)))
                  (lambda () (maak-actieve-torpedo (level-adt 'incr-aantal-beschikbare-torpedos!)))))
        ;; de powerups die niet tijdgebaseerd zijn.
        (niet-tijdgebaseerde (vector levens-hersteller torpedo)))
    
    (define (geef-actieve-powerup-terug type)
      (vector-ref actieve-powerups-adts type))

    (define (niet-tijdgebaseerde-powerup? type)
      (find-in-vector niet-tijdgebaseerde type =))
      

    (define (dispatch msg)
      (cond ((eq? msg 'geef-actieve-powerup-terug) geef-actieve-powerup-terug)
            ((eq? msg 'niet-tijdgebaseerde-powerup?) niet-tijdgebaseerde-powerup?)
            (else (error "onbekend bericht" msg))))
    dispatch))