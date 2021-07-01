;----------------- Explosie ADT ------------------;
(define (maak-explosie obj-positie)
  (let ((explosie-tijd 0)
        (verwijder? #f))

    (define (set-explosie-tijd! nieuw)
      (set! explosie-tijd nieuw))

    (define (toon-explosie! delta-tijd)
      (set-explosie-tijd! (+ delta-tijd explosie-tijd))
      (if (> explosie-tijd explosie-duur)
          (set! verwijder? #t)))
    
    (define (dispatch msg)
      (cond ((eq? msg 'positie) obj-positie)
            ((eq? msg 'toon-explosie!) toon-explosie!)
            ((eq? msg 'verwijder?)  verwijder?)
            (else (error "onbekend bericht" msg))))
    
    dispatch))