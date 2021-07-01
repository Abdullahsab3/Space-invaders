;----------- Positie ADT -----------;
(define (maak-positie x y)

  
  (define (set-x! nieuwe-x)
    (set! x nieuwe-x))
  (define (set-y! nieuwe-y)
    (set! y nieuwe-y))

  (define (beweeg! symbol)
   (cond ((eq? symbol 'rechts) (set-x! (+ x 1)))
         ((eq? symbol 'links) (set-x! (- x 1)))
         ((eq? symbol 'omhoog) (set-y! (- y 1)))
         ((eq? symbol 'omlaag) (set-y! (+ y 1)))))
  
  
  (define (vergelijk? positie)
    (and (= x (positie 'get-x))
         (= y (positie 'get-y))))

  (define (dispatch-positie msg)
    (cond ((eq? msg 'get-x) x)
          ((eq? msg 'get-y) y)
          ((eq? msg 'set-x!) set-x!)
          ((eq? msg 'set-y!) set-y!)
          ((eq? msg 'beweeg!) beweeg!)
          ((eq? msg 'vergelijk?) vergelijk?)
          (else (error "onbekend bericht" msg))))
  dispatch-positie)
          