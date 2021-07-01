;-------- Actieve torpedo powerup -----------;
(define (maak-actieve-torpedo incr-aantal-beschikbare-torpedos!)
  (let ((parent (maak-parent-actieve-powerup torpedo)))

    (define (voer-actie-uit!)
      (incr-aantal-beschikbare-torpedos!)
      (parent 'deactiveer!))

    (define (dispatch msg)
      (cond ((eq? msg 'voer-actie-uit!) voer-actie-uit!)
            (else (parent msg))))
    dispatch))
  