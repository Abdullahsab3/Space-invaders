;-------------- actieve raketscutum ADT ---------------;
(define (maak-actieve-raketscutum)
  (let ((parent (maak-parent-actieve-powerup raketscutum))
        (raketscutum-tijd 0))

    ; zolang dat een raketscutum object in de lijst van actieve powerups in het level ADT zit ..
    ;; .. blijft de raketscutum geactiveerd.
    (define (activeer-raketscutum! delta-tijd)
      (set! raketscutum-tijd (+ raketscutum-tijd delta-tijd))
      (if (> raketscutum-tijd raketscutum-duur)
          (begin (set! raketscutum-tijd 0)
                 (parent 'deactiveer!))))

    (define (dispatch msg)
      (cond ((eq? msg 'voer-actie-uit!) activeer-raketscutum!)
            (else (parent msg))))
    dispatch))