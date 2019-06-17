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
(define (top stack) (car stack))
(define (pop stack) (cdr stack))


;-------------------------------------------------------------------------------------------------------------------------------------------------

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.

; nu mergea list
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (cons co-varnames (cons co-consts (cons co-names (cons co-code (cons stack (cons IC empty-stack)))))))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine) (car stack-machine))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine) (cadr stack-machine))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))

;; 'dummy-co-names
(define (get-names stack-machine) (caddr stack-machine))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine) (cadddr stack-machine))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine) (cadddr (cdr stack-machine)))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine) (cadddr (cddr stack-machine)))




;---------------------------------------------------------------------------------------------------------------------------------------------------

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

(define (get-symbol-index symbol)
  (cond ((eq? symbol 'CO-VARNAMES) '0) ((eq? symbol 'CO-CONSTS) '1) ((eq? symbol 'CO-NAMES) '2) ((eq? symbol 'CO-CODE) '3) ((eq? symbol 'STACK) '4) (else '5)))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (let* ((s (get-symbol-index symbol))
        (first-part (take stack-machine s))
        (last-part (drop stack-machine (add1 s))))
  (append first-part (cons item last-part))))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (let ((stack (get-stack stack-machine)))
  (update-stack-machine (push value stack) 'STACK stack-machine)))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (let ((stack (get-stack stack-machine)))
  (update-stack-machine (pop stack) 'STACK stack-machine)))


;---------------------------------------------------------------------------------------------------------------------------------------------------

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.

; creierul programului
;- la fiecare apel recursiv intoarce un nou stack-machine modificat de instr curenta
;- instructiunea se ia din co-codes, avanseaza incrementand IC
(define (run-stack-machine stack-machine)
  (let ((IC (get-IC stack-machine))
        (code (get-code stack-machine)))
  (if (eq? IC (length code))
      stack-machine
      (run-stack-machine (switch-f (car (get-instr-crt stack-machine))
                                   (cdr (get-instr-crt stack-machine))
                                   (update-stack-machine (add1 IC) 'IC stack-machine))))))

; primeste un element din co-codes si intoarce functia corespunzatoare
(define (switch-f fun indice stack-machine)
  (cond
    ((equal? fun (get-opname 1)) (pop-top indice stack-machine))
    ((equal? fun (get-opname 22)) (binary-modulo stack-machine))
    ((equal? fun (get-opname 23)) (binary-add stack-machine))
    ((equal? fun (get-opname 24)) (binary-substract stack-machine))
    ((equal? fun (get-opname 55)) (inplace-add stack-machine))
    ((equal? fun (get-opname 56)) (inplace-substract stack-machine))
    ((equal? fun (get-opname 59)) (inplace-modulo stack-machine))
    ((equal? fun (get-opname 68)) (get-iter stack-machine))
    ((equal? fun (get-opname 83)) (return-value stack-machine))
    ((equal? fun (get-opname 87)) (pop-block indice stack-machine))
    ((equal? fun (get-opname 93)) (for-iter indice stack-machine))
    ((equal? fun (get-opname 100)) (load-const indice stack-machine))
    ((equal? fun (get-opname 107)) (compare-op (comparator indice) stack-machine))
    ((equal? fun (get-opname 113)) (jump-absolute indice stack-machine))
    ((equal? fun (get-opname 114)) (pop-jump-if-false indice stack-machine))
    ((equal? fun (get-opname 116)) (load-global indice stack-machine))
    ((equal? fun (get-opname 120)) (setup-loop indice stack-machine))
    ((equal? fun (get-opname 124)) (load-fast indice stack-machine))
    ((equal? fun (get-opname 125)) (store-fast indice stack-machine))))

; returneaza co-codes(IC)
(define (get-instr-crt stack-machine)
  (let ((IC (get-IC stack-machine))
        (code (get-code stack-machine)))
  (list-ref code IC)))

(define (pop-top indice stack-machine)
  (let ((stack (get-stack stack-machine)))
    (car (stack))))

(define (binary-modulo stack-machine)
  (let* ((stack (get-stack stack-machine))
         (x (top (pop stack)))
         (y (top stack)))
    (update-stack-machine (push (remainder x y) (pop (pop stack))) 'STACK stack-machine)))

(define (binary-add stack-machine)
  (let* ((stack (get-stack stack-machine))
         (x (top (pop stack)))
         (y (top stack)))
    (update-stack-machine (push (+ x y) (pop (pop stack))) 'STACK stack-machine)))

(define (binary-substract stack-machine)
  (let* ((stack (get-stack stack-machine))
         (x (top (pop stack)))
         (y (top stack)))
    (update-stack-machine (push (- x y) (pop (pop stack))) 'STACK stack-machine)))

(define (inplace-add stack-machine) (binary-add stack-machine))
(define (inplace-substract stack-machine) (binary-substract stack-machine))
(define (inplace-modulo stack-machine) (binary-modulo stack-machine))

(define (get-iter stack-machine) stack-machine)
(define (return-value stack-machine) stack-machine)
(define (pop-block indice stack-machine) stack-machine)

(define (for-iter delta stack-machine)
  (if (null? (top (get-stack stack-machine)))
      (pop-exec-stack (jump-absolute (+ (+ delta 2) (* (get-IC stack-machine) 2)) stack-machine)) 
      (let ((a (car (top (get-stack stack-machine)))) (b (cdr (top (get-stack stack-machine)))))
        (push-exec-stack a (push-exec-stack b (pop-exec-stack stack-machine))))))

(define (load-const indice stack-machine)
  (let ((consts (get-consts stack-machine)))
  (push-exec-stack (hash-ref consts indice) stack-machine)))

(define (compare-op comparator stack-machine)
  (let* ((stack (get-stack stack-machine))
        (result (compare comparator stack)))
    (update-stack-machine (push result (cddr stack)) 'STACK stack-machine)))

(define (compare comparator stack)
  (let ((x (car stack))
        (y (cadr stack)))
  (cond
    ((equal? comparator '<) (< y x))
    ((equal? comparator '<=) (<= y x))
    ((equal? comparator '==) (= y x))
    ((equal? comparator '!=) (not (= y x)))
    ((equal? comparator '>) (> y x))
    ((equal? comparator '>=) (>= y x)))))

; lista cu comparatori posibili
(define comparatori '(< <= == != > >=))

; returneaza comparatori(i)
(define (comparator i)
  (list-ref comparatori i))

(define (jump-absolute indice stack-machine)
  (update-stack-machine (/ indice 2) 'IC stack-machine))

(define (pop-jump-if-false indice stack-machine)
  (if (equal? (top (get-stack stack-machine)) #f)
      (pop-exec-stack (update-stack-machine (/ indice 2) 'IC stack-machine))
      (pop-exec-stack stack-machine)))


(define (load-global indice stack-machine) stack-machine)
(define (setup-loop indice stack-machine) stack-machine)

(define (load-fast indice stack-machine)
  (let ((varnames (get-varnames stack-machine)))
    (push-exec-stack (hash-ref varnames indice) stack-machine)))

(define (store-fast indice stack-machine)
  (let ((stack (get-stack stack-machine))
        (varnames (get-varnames stack-machine)))
  (update-stack-machine (pop stack) 'STACK
  (update-stack-machine (hash-set varnames indice (top stack)) 'CO-VARNAMES stack-machine))))

