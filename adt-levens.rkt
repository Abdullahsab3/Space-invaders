;--------------- Leven ADT ----------;
(define (maak-levens-ADT levens)
        
  (define (set-levens! nieuw)
    (set! levens nieuw))

  (define (verminder-levens!)
    (if (not (nul-levens?))
        (set-levens! (- levens 1))))

  (define (nul-levens?)
    (zero? levens))

  (define (dispatch-levens msg)
    (cond ((eq? msg 'get-levens) levens)
          ((eq? msg 'set-levens!) set-levens!)
          ((eq? msg 'verminder-levens!) (verminder-levens!))
          ((eq? msg 'nul-levens?) (nul-levens?))
          (else (error "Onbekend bericht" msg))))

  dispatch-levens)