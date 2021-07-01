;-------------------- Spel ADT -------------------;
(define (maak-adt-spel)
  (let* ((level-adt (maak-adt-level initieel-level
                                    spel-breedte spel-hoogte))
        (teken-adt (maak-teken venster-breedte-px venster-hoogte-px)))


    
    (define (herstart-spel!)
      (set! level-adt (maak-adt-level initieel-level
                                      spel-breedte spel-hoogte))
      (teken-adt 'herteken!))

    
    ;; luisteren naar toetsinvoer
    (define (toets-procedure status toets)    
      (if (eq? status 'pressed)
          ;; als de speler verliest, kan het spel herstart worden door op Enter te drukken.
          (if (eq? toets #\return)
              (if (spel-verloren?)
                  (herstart-spel!))
              ((level-adt 'toets!) toets))))
      
    
    (define (spel-verloren?)
      (level-adt 'spel-verloren?))
    
    ;; de spellus laten draaien
    (define (spel-lus-procedure delta-tijd)
      
      (cond ((level-adt 'level-gewonnen?) (level-adt 'level-up!)
                                          ;; dit is voor het geval wanneer de cheatknop P ingedrukt is,
                                          ;; dan gaat het spel naar het volgende level gaan
                                          ;; en moeten alle aliens uit het vorige level direct verwijderd worden
                                          (teken-adt 'verwijder-alle-aliens!))
            ((spel-verloren?) (teken-adt 'teken-gameover!))
            (else
             ((level-adt 'update!) delta-tijd)
             ((teken-adt 'teken-spel!) dispatch-spel))))

    ;; het spel starten
    (define (start)
      ((teken-adt 'set-spel-lus-functie!) spel-lus-procedure)
      ((teken-adt 'set-toets-functie!) toets-procedure))

    (define (dispatch-spel msg)
      (cond ((eq? msg 'start) start)
            ((eq? msg 'level) level-adt)
            (else (error "onbekende boodschap" msg))))
    
    dispatch-spel))
