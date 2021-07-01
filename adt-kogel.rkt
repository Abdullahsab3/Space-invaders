;------------------------------ Kogel ADT -------------------------------;
;; obj-positie is de positie van het object waarvan de kogel vertrokken is.
(define (maak-kogel obj-positie richting)
  ;; een boolean wordt als lokale toestand opgeslagen ..
  ;; .. om te weten wanneer de kogel verwijderd moet worden.
  (let ((positie (maak-positie (obj-positie 'get-x)
                               (obj-positie 'get-y)))
        (verwijder? #f)
        (kogel-tijd 0))
  
    (define (verwijder!)
      (set! verwijder? #t))

    (define (set-tijd! nieuw)
      (set! kogel-tijd nieuw))

    (define (beweeg!)
      ((positie 'beweeg!) richting))

    ;; verwijder de kogel als hij buiten het scherm komt te staan.
    (define (buiten-het-scherm!)
      (let ((y-waarde (positie 'get-y)))
        (if (or (> y-waarde bodem)
                (< y-waarde bovenkant))
            (verwijder!))))


    (define (beweeg-kogel! delta-tijd)
      (set-tijd! (+ delta-tijd kogel-tijd))
      (if (> kogel-tijd kogel-snelheid)
          (begin
            (beweeg!)
            (buiten-het-scherm!)
            (set-tijd! 0))))


    (define (dispatch-kogel msg)
      (cond ((eq? msg 'positie) positie)
            ((eq? msg 'richting) richting)
            ((eq? msg 'beweeg!) beweeg-kogel!)
            ((eq? msg 'verwijder?) verwijder?)
            ((eq? msg 'verwijder!) (verwijder!))
            (else (error "Onbekend bericht" msg))))
    dispatch-kogel))