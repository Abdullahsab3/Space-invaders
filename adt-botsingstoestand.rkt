;---------- botsing status ADT ------------;
;; De taak van dit ADT is de status van de botsing van het object bijhouden
;; en de nodige aanpassingen inbrengen indien het object gebotst heeft.
(define (maak-botsingstoestand positie)
  (let ((botsingstoestand #f))

    
    (define (is-geraakt? antipos)
      (let ((check ((positie 'vergelijk?) antipos)))
        ;; indien het wel geraakt is, wordt de botsingstoestand variabele direct op #t gezet.
        (if check (set! botsingstoestand #t))
        check))

    ;; de actie wordt al geevalueerd als argument wanneer deze procedure opgeroepen wordt.
    ;; in de objecten zoals de raket geven we aan deze procedure een procedure-oproep mee ..
    ;; .. die dus geevalueerd wordt wanneer deze procedure opgeroepen wordt.
    (define (pas-aan-indien-geraakt! actie)
      (set! botsingstoestand #f)
      (actie))

    (define (dispatch msg)
      (cond ((eq? msg 'is-geraakt?) is-geraakt?)
            ((eq? msg 'pas-aan-indien-geraakt!) pas-aan-indien-geraakt!)
            (else (error "onbekend bericht" msg))))
    dispatch))

