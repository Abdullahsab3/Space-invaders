;------------------- Headed list -------------------;

; symbol -> pair
(define (make-headed-list tag)
  (cons tag '()))

; pair -> pair                                     
(define (content headed-list)
  (cdr headed-list))

; any pair -> /
(define (add-to-headed-list! el headed-list)
  (set-cdr! headed-list (cons el (content headed-list))))

;; een variant op het headed list ADT: headed list met een counter
;; Deze variant werd gebruikt om te debuggen
(define (make-headed-list-with-counter tag)
  (cons (cons tag 0) '()))

(define (add-to-headed-list-with-counter! el hlst)
  (add-to-headed-list! el hlst)
  (incr-aantal! hlst))

; pair -> number
(define (aantal headed-list)
  (cdar headed-list))


(define (find-in-headed-list lst el ==?)
  (define (iter lst)
  (cond ((null? lst) #f)
        ((==? (car lst) el) (car lst))
        (else (iter (cdr lst)))))
  (iter (content lst)))


; pair -> symbol
(define (tag headed-list)
  (caar headed-list))
; pair -> /
(define (incr-aantal! headed-list)
  (set-cdr! (car headed-list)
            (+ 1 (aantal headed-list))))
; pair -> /
(define (decr-aantal! headed-list)
  (set-cdr! (car headed-list)
            (- (aantal headed-list) 1)))
  
;; Uit het vak structuur van computerprogramma's 1 - Les 10 muteerbare Data - dia 25 en verder.
;; any pair -> /
(define (remove-from-headed-list! el headed-list)
  (cond
    ((null? headed-list) 'done)
    ((null? (cdr headed-list)) 'done)
    ((eq? el (cadr headed-list))
     (set-cdr! headed-list (cddr headed-list)))
   ;  (remove-from-headed-list! el headed-list))
    (else
     (remove-from-headed-list! el (cdr headed-list)))))

(define (for-each-in-headed-list proc headed-list)
  (for-each proc (content headed-list)))


;-------------------- EEN MATRIX ADT ---------------------;
;; een matrix aanmaken
(define (make-matrix rows cols fillobj)
  (let ((matrix (make-vector rows 'vec-placeholder)))
    (let make-matrix-loop ((idx 0))
      (cond ((<= idx (- rows 1))
            (vector-set! matrix idx (make-vector cols fillobj))
            (make-matrix-loop (+ idx 1)))))
    matrix))

;; reffen in de matrix
(define (matrix-ref mat rows cols)
  (vector-ref (vector-ref mat rows) cols))

;; setten in de matrix
(define (matrix-set! mat rows cols new)
  (vector-set! (vector-ref mat rows) cols new))

;; twee elementen swappen in de matrix
;; gegeven hun coordinaten
(define (matrix-swap m x1 y1 x2 y2)
  (let ((old (matrix-ref m x1 y1)))
    (matrix-set! m x1 y1 (matrix-ref m x2 y2))
    (matrix-set! m x2 y2 old)))

;; aantal rijen in de matrix
(define (matrix-rows matrix)
  (vector-length matrix))

;; aantal kolommen in de matrix
(define (matrix-cols matrix)
  (vector-length (vector-ref matrix 0)))

;; een element vinden in de vector en zijn idx teruggeven.
(define (find-in-vector vec el ==?)
  (let loop ((idx 0))
    (if (= idx (vector-length vec))
        #f
    (let ((curr (vector-ref vec idx)))
      (if (==? curr el)
          idx
          (loop (+ idx 1)))))))

;; een element vinden in de vector en het element zelf teruggeven.
(define (find-content-in-vector vec el ==?)
  (let loop ((idx 0))
    (if (= idx (vector-length vec))
        #f
    (let ((curr (vector-ref vec idx)))
      (if (==? curr el)
          curr
          (loop (+ idx 1)))))))

;; een element vinden in de matrix en zijn coordinaten teruggeven in een conscel.
(define (find-in-matrix mat el ==?)
  (let outerloop ((row 0))
    (if (= row (matrix-rows mat))
        #f
    (let ((col (find-in-vector (vector-ref mat row) el ==?)))
    (cond (col (cons row col))
          (else (outerloop (+ row 1))))))))
        
;; een element verwijderen uit de matrix
(define (remove-from-matrix! mat el)
  (let* ((el-cdtes (find-in-matrix mat el eq?))
         (row (car el-cdtes))
         (col (cdr el-cdtes)))
    (matrix-set! mat row col 'deleted)))

;; een procedure mappen op een vector
(define (vector-map f vec)
  (define vector-out (make-vector (vector-length vec)))
  (let loop ((idx 0))
    (if (= idx (vector-length vec))
        vector-out
        (let ((orig-el (vector-ref vec idx)))
          (vector-set! vector-out idx (f orig-el))
          (loop (+ idx 1))))))

;; een procedure for-eachen op een vector
(define (vector-for-each f vec)
  (let loop ((idx 0))
    (if (not (= idx (vector-length vec)))
        (let ((orig-el (vector-ref vec idx)))
          (f orig-el)
          (loop (+ idx 1))))))

;; een procedure for-eachen op een matrix
(define (matrix-for-each matrix proc)
  (vector-for-each (lambda (vecjes)
                     (vector-for-each proc vecjes))
                   matrix))
        
;; een procedure mappen op een matrix
(define (matrix-map matrix proc)
  (vector-map (lambda (vecjes) (vector-map proc vecjes)) matrix))

;; een willekeurig element uit de matrix halen
(define (random-element matrix)
  (let ((random-row (random (- (matrix-rows matrix) 1)))
        (random-col (random (- (matrix-cols matrix) 1))))
    (matrix-ref matrix random-row random-col)))

;; een procedure toepassen op de omliggende elementen van welbepaalde coordinaaten.
(define (for-each-on-surrounding matrix cdtes f)
  (define els (make-headed-list 'els))
  (let ((which-row (car cdtes))
        (which-col (cdr cdtes)))

    ;; checken naar de randgevallen tov de kolom
    (cond ((< 0 which-col (- (matrix-cols matrix) 1))
           (let ((el-right (matrix-ref matrix which-row (+ which-col 1)))
                 (el-left (matrix-ref matrix which-row (- which-col 1))))
             (add-to-headed-list! el-right els)
             (add-to-headed-list! el-left els)))
          ((zero? which-col)
           (let ((el-right (matrix-ref matrix which-row (+ which-col 1))))
             (add-to-headed-list! el-right els)))
          ((= which-col (- (matrix-cols matrix) 1))
           (let ((el-left (matrix-ref matrix which-row (- which-col 1))))
             (add-to-headed-list! el-left els))))
    
    ;; checken voor de randgevallen tov de rij
    (cond ((< 0 which-row (- (matrix-rows matrix) 1))
           (let ((el-up (matrix-ref matrix (- which-row 1) which-col))
                 (el-down (matrix-ref matrix (+ which-row 1) which-col)))
             (add-to-headed-list! el-up els)
             (add-to-headed-list! el-down els)))
          ((zero? which-row)
           (let ((el-down (matrix-ref matrix (+ which-row 1) which-col)))
             (add-to-headed-list! el-down els)))
          ((= which-row (-  (matrix-rows matrix) 1))
           (let ((el-up (matrix-ref matrix (- which-row 1) which-col)))
             (add-to-headed-list! el-up els))))
    
    ;; de procedure toepassen op alle gevonden elementen
    (for-each-in-headed-list
     (lambda (el)
       (if (not (eq? el DELETED))
           (f el)))
     els)))


; display matrix 
; space? is een predikaat (als #t zullen er spaties tussen de elementen zitten)
(define (display-matrix matrix space?)
  (define (display-vector vec space?)
    (vector-for-each (lambda (el)
                       (display el)
                       (if space? (display " "))) vec))
  (vector-for-each (lambda (vector)
                     (display-vector vector space?) (newline))
                   matrix))


;; een willekeurige booleanse waarde teruggeven volgens een bepaalde frequentie
(define (random-boolean freq)
  (zero? (random freq)))



(define (last-cell lst)
  (if (null? (cdr lst))
      lst
      (last-cell (cdr lst))))

;; een destructieve append 
(define (append! lst1 lst2)
  (set-cdr! (last-cell lst1) lst2)
  lst1)


;; extra hulpprocedure om te abstraheren
(define (half num)
  (quotient num 2))

(define (double num)
  (* 2 num))
