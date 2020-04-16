#lang racket
;-----------------------------------------------------------;
;                         CONSTANT                          ;
;-----------------------------------------------------------;

(define pi (* 4 (atan 1 1)))

;-----------------------------------------------------------;
;                      BASIC FUNCTIONS                      ;
;-----------------------------------------------------------;

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;-----------------------------------------------------------;
;                          FACTORY                          ;
;-----------------------------------------------------------;

;Prend des réels en entré et retourne leur somme
(define Add
  (lambda a
    (apply + a)))

;Prend des réels en entrée et retourne leur produit
(define Mult
  (lambda a
    (apply * a)))

;Prend 2 réels en entrée, a et b, et retourne a exposé en b
(define Pow
  (lambda (a b)
    (expt a b)))

;Prend un réel en entrée et retourne le cosinus de ce nombre (en radian)
(define Cos
  (lambda (a)
    (cos a)))

;Prend un réel en entrée et retourne le sinus de ce nombre (en radian)
(define Sin
  (lambda (a)
    (sin a)))

;-----------------------------------------------------------;
;                        SIMPLIFICATION                     ;
;-----------------------------------------------------------;

(define sf-add
  (lambda (args)
    (args)))


;-----------------------------------------------------------;
;                            NODE                           ;
;-----------------------------------------------------------;

; Prend en entrée une VAR_STORE et une table d'assignation et retourne le nombre
; correspondant a la VAR_STORE si cette dernière se trouve dans la table
; d'assignation. Si le symbole n'est pas présent dans la table d'assignation,
; retourne le symbol.
(define replace-var
  (lambda (x ls)
    (cond ((null? ls) x)
          ((equal? (caar ls) x) (cdar ls))
          (else (replace-var x (cdr ls))))))


; Prend en entrée un FN_STORE 'tri' et une table d'assignation 't' et retourne un NODE
; représentant le graphe de calculs simplifié où toutes les occurences des variables présentent
; dans la table 't' ont été remplacées par leur valeur correspondante.
(define replace-fn
  (lambda (tri t)
    (list (car tri))))
          

; Prend en entrée un STORE 'val' et une table d'assignation 't' et retourne ce même STORE dont les
; symboles ont été remplacés par les valeurs correspondantes dans la table d'assignation.
; Si un symbole n'est pas présent, ce dernier reste comme tel dans le STORE.
(define replace
  (lambda (val t)
    (cond ((or (number? val) (null? t)) val)  ;val est un nombre (CST_STORE) ou nd_arg est null
          ((symbol? val) (replace-var val t)) ;val est un symbole (VAR_STORE)
          (else (replace-fn val t)))))        ;val est un triplet (FN_STORE)
    
                

(define node
  (lambda (nd_arg)
    (lambda ()
      (if (list? nd_arg) (replace val nd_arg)
          #f))))

(define recover
  (lambda (nd)
    (

(cadr '(+ ((1 2) (3 4) (5 6)) 'x))
(list 'a 'b)