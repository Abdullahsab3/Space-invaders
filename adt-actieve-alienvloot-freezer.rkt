;-------- Actieve alienvloot freezer ADT -------------;
(define (maak-actieve-alienvlootfreezer)
  (let ((parent (maak-parent-actieve-powerup alienvlootfreezer))
        (alienvloot-freeze-tijd 0))
    
    
    (define (freeze-alienvloot! delta-tijd)
      ; de snelheid van de alienvloot aanpassen naar 10 seconden.
      ; op die manier wordt de vloot bevrozen voor 10 seconden.
      (set! alienvloot-snelheid vloot-stil-voor-10-secs)
      (set! alienvloot-freeze-tijd (+ alienvloot-freeze-tijd delta-tijd))
      (if (> alienvloot-freeze-tijd alienvloot-freeze-duur)
          (begin (set! alienvloot-snelheid initiele-alienvloot-snelheid)
                 (set! alienvloot-freeze-tijd 0)
                 (parent 'deactiveer!))))

    (define (dispatch msg)
      (cond ((eq? msg 'voer-actie-uit!) freeze-alienvloot!)
            (else (parent msg))))
    
    dispatch))