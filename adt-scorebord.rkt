;----------- Scorebord ADT ---------;
(define (maak-scorebord-adt)
  
  (define (get-hoogste-score)
    ;; De hoogste score zit opgeslagen in een bestand genaamd hoogstescore.txt
    (let* ((input (open-input-file "hoogstescore.txt"))
           (input-content (read input)))
      (close-input-port input)
      input-content))
  
  (let ((huidige-score 0)
        (hoogste-score (get-hoogste-score))
        (verdubbel? #f))
    
    
    (define (update-hoogste-score-bestand num)
      (let ((output (open-output-file "hoogstescore.txt" #:exists 'replace)))
        (write num output)
        (close-output-port output)))
    
    (define (pas-huidige-score-aan! alien-kleur)
      (set! huidige-score   (let* ((aliens-score (cond ((eq? alien-kleur 'geel) score-gele-aliens)
                                                       ((eq? alien-kleur 'paars) score-paarse-aliens)
                                                       ((eq? alien-kleur 'groen) score-groene-aliens)
                                                       (else 0)))
                                   ;; als verdubbel? aanstaat (true is), dan wordt de score van de alien verdubbeld.
                                   (score (+ huidige-score (if verdubbel?
                                                               (* aliens-score 2)
                                                               aliens-score))))
                              score))
      
      ;; er wordt gecheckt of de huidige score hoger is de hoogste score
      (nieuwe-hoogste-score!))

    (define (nieuwe-hoogste-score!)
      (if (> huidige-score hoogste-score)
          (begin (update-hoogste-score-bestand huidige-score)
                 (set! hoogste-score huidige-score))))

    (define (verdubbel-aan!)
      (set! verdubbel? #t))

    (define (verdubbel-uit!)
      (set! verdubbel? #f))
    


    (define (dispatch-scorebord msg)
      (cond ((eq? msg 'huidige-score) huidige-score)
            ((eq? msg 'pas-huidige-score-aan!) pas-huidige-score-aan!)
            ((eq? msg 'hoogste-score) hoogste-score)
            ((eq? msg 'verdubbel-aan!) (verdubbel-aan!))
            ((eq? msg 'verdubbel-uit!) (verdubbel-uit!))
            (else (error "onbekend bericht" msg))))
    
    dispatch-scorebord))