;--------- Actieve super-kogel ADT ----------;
(define (maak-actieve-super-kogel)
  (let ((super-kogel-tijd 0)
        (parent (maak-parent-actieve-powerup super-kogel)))

    (define (set-super-kogel-tijd! nieuw)
      (set! super-kogel-tijd nieuw))
    
    (define (activeer-super-kogel! delta-tijd)
      (set-super-kogel-tijd! (+ super-kogel-tijd delta-tijd))
      (if (> super-kogel-tijd super-kogel-duur)
          (begin (set! super-kogel-tijd 0)
                 (parent 'deactiveer!))))

    (define (dispatch msg)
      (cond ((eq? msg 'voer-actie-uit!) activeer-super-kogel!)
            (else (parent msg))))
    
    dispatch))