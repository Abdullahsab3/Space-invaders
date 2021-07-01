;------------ actieve scoreverdubbelaar ADT ----------;
(define (maak-actieve-scoreverdubbelaar scorebord)
  (let ((parent (maak-parent-actieve-powerup scoreverdubbelaar))
        (scoreverdubbelaar-tijd 0))

    
    (define (activeer-scoreverdubbelaar! delta-tijd)
        (scorebord 'verdubbel-aan!)
        (set! scoreverdubbelaar-tijd (+ scoreverdubbelaar-tijd delta-tijd))
        (if (> scoreverdubbelaar-tijd  scoreverdubbelaar-duur)
            (begin (set! scoreverdubbelaar-tijd 0)
                   (scorebord 'verdubbel-uit!)
                   (parent 'deactiveer!))))
    
    (define (dispatch msg)
      (cond ((eq? msg 'voer-actie-uit!) activeer-scoreverdubbelaar!)
            (else (parent msg))))
    dispatch))