;--------- acteve raketlevenshersteller -----------;
(define (maak-actieve-raket-levenshersteller raket-levensobject)
  (let ((parent (maak-parent-actieve-powerup levens-hersteller)))

    
    (define (herstel-raketlevens!)
      ; het huidig aantal levens van de raket wordt terug op de initiele raket levens gezet.
      ((raket-levensobject 'set-levens!) initiele-raket-levens)
      (parent 'deactiveer!))

    (define (dispatch msg)
      (cond ((eq? msg 'voer-actie-uit!) herstel-raketlevens!)
            (else (parent msg))))
    dispatch))