#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)
;-------------------------------------------------------------------------------------------------------------------------------------------------

;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack '())
(define (make-stack) (list))

(define (push element stack) (cons element stack))
(define (top stack) (first stack))
(define (pop stack) (rest stack))

;-------------------------------------------------------------------------------------------------------------------------------------------------

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  ;'your-code-here)
  (push co-varnames (push co-consts (push co-names (push co-code (push stack (push IC empty-stack)))))))

;----------------------------------------------------------------------------------------------  
;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă


;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine) (top stack-machine))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine) (top (pop stack-machine)))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine) (top (pop (pop stack-machine))))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine) (top (pop (pop (pop stack-machine)))))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine) (top (pop (pop (pop (pop stack-machine))))))

  
;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine) (top (pop (pop (pop (pop (pop stack-machine)))))))


(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;-------------------------------------------------------------------------------------------------------------------------------------------------
  
;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  ;'your-code-here)
  (let ((symbol? 'CO-VARNAMES)) '1)
  (let ((symbol? 'CO-CONSTS)) '2)
  (let ((symbol? 'CO-NAMES)) '3)
  (let ((symbol? 'CO-CODE)) '4)
  (let ((symbol? 'STACK)) '5)
  (let ((symbol? 'INSTRUCTION-COUNTER)) '6)
)

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"


(define (update-stack-machine item symbol stack-machine)
  ;'your-code-here)
  (let loop (( x 1 ) ( le '()))
    (cond
      [( > x (length stack-machine)) (reverse le)]
      [(= x (get-symbol-index symbol) )
          (loop (add1 x) (cons item le) )]
      [else
      (loop (add1 x) (cons (list-ref stack-machine (sub1 x)) le))]))
)
;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  ;'your-code-here)
  (update-stack-machine (push value (get-stack stack-machine)) 'STACK stack-machine))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
 ; 'your-code-here)
   (update-stack-machine (pop (get-stack stack-machine)) 'STACK stack-machine))
  
;-------------------------------------------------------------------------------------------------------------------------------------------------

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.
(define (run-stack-machine stack-machine)
  'your-code-here)

;-------------------------------------------------------------------------------------------------------------------------------------------------
