;-------- parent actieve powerup ADT ------;
(define (maak-parent-actieve-powerup type)
  (let ((verwijder? #f))

    (define (deactiveer!)
      (set! verwijder? #t))

    (define (activeer!)
      (set! verwijder? #f))
    
    (define (dispatch msg)
      (cond ((eq? msg 'verwijder?) verwijder?)
            ((eq? msg 'activeer!) (activeer!))
            ((eq? msg 'deactiveer!) (deactiveer!))
            ((eq? msg 'get-type) type)
            (else (error "onbekend bericht" msg))))
    
    dispatch))