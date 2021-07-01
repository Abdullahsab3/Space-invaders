;----------------------- Raket ADT ----------------------;
(define (maak-raket positie)
  
  (let ((levensobj (maak-levens-ADT initiele-raket-levens))
        (botsingstoestand (maak-botsingstoestand positie)))

    (define (beweeg-horizontaal! richting) 
            (let ((x-waarde (positie 'get-x)))
                   (cond ((eq? richting 'right)
                          ;; De raket mag naar rechts gaan zolang de rechterzijkant niet geraakt is.
                          (if (< x-waarde rechterzijkant)
                              ((positie 'beweeg!) 'rechts)))
                         ((eq? richting 'left)
                          ;; De raket mag naar links gaan zolang de linkerzijkant niet geraakt is.
                          (if (< linkerzijkant x-waarde)
                              ((positie 'beweeg!) 'links))))))
    ; Deze procedure maakt een kogel-object aan en geeft hem terug.$
    ;; / -> kogel ADT
    (define (schiet!)
      (let ((kogel (maak-kogel positie 'omhoog)))
        kogel))

  (define (is-geraakt? antipos)
    ((botsingstoestand 'is-geraakt?) antipos))

  (define (pas-aan-indien-geraakt!)
    ((botsingstoestand 'pas-aan-indien-geraakt!)
     (lambda ()
       (levensobj 'verminder-levens!))))

  
    (define (dispatch-raket msg)
      (cond ((eq? msg 'positie) positie)
            ((eq? msg 'schiet!) (schiet!))
            ((eq? msg 'beweeg-horizontaal!) beweeg-horizontaal!)
            ((eq? msg 'get-levensobject) levensobj)
            ((eq? msg 'geraakt?) is-geraakt?)
            ((eq? msg 'pas-aan-indien-geraakt!) (pas-aan-indien-geraakt!))
            (else (error "onbekend bericht" msg))))
    dispatch-raket))