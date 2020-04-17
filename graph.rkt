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
;                            NODE                           ;
;-----------------------------------------------------------;

; Prend en entrée une VAR_STORE 'var' et une table d'assignation 't' et retourne le nombre
; correspondant a la VAR_STORE si cette dernière se trouve dans la table
; d'assignation. Retourne le symbole sinon.
(define replace-var
  (lambda (var t)
    (cond ((null? t) var)
          ((equal? (caar t) var) (cdar t))
          (else (replace-var var (cdr t))))))


; Prend en entrée 3 listes de NODEs, place les NODEs de 'nodes' qui ont un STORE de type CST_STORE
; dans la liste 'cst', les NODEs de 'nodes' qui ont un STORE de type VAR_STORE ou FN_STORE dans
; la liste 'var' et retourne une listes de NODEs dont les premiers éléments sont les NODEs de 'cst'
; suivies des NODEs de 'nodes' ajoutées à 'cst' (dans le même ordre que celui de la liste 'nodes' 
; initialement donnée) et dont les derniers éléments sont les NODEs de la liste 'var' suivies des NODEs
; de 'nodes' ajoutées à 'var' (dans le même ordre que celui de la liste 'nodes' initialement donnée)
(define re-order
  (lambda (nodes cst var)
    (if (null? nodes) (append (reverse cst) (reverse var))
        (let ((nd (car nodes)))
          (if (number? (nd)) (re-order (cdr nodes) (cons nd cst) var)
              (re-order (cdr nodes) cst (cons nd var)))))))


; Prend en entrée un FN_STORE 'tri' et une table d'assignation 't' et retourne ce même
; FN_STORE ordonné (constantes en premiers fils) où toutes les occurences des variables
; présentes dans la table 't' ont été remplacées dans le graphe par leur valeur correspondante.
(define replace-fn
  (lambda (tri t)
     (map (replace t) (cadr tri))))
                    

; Prend en entrée un NODE 'node' et une table d'assignation 't' et retourne un NODE dont le STORE
; est celui donné en entrée dont les symboles ont été remplacés par les valeurs correspondantes dans   
; la table d'assignation. Si un symbole n'est pas présent, ce dernier reste comme tel dans le STORE.
(define replace
  (lambda (t)
    (lambda (nd)
      (let ((store (nd)))
        (cond ((or (number? store) (null? t)) (node store))  ; val est un nombre (CST_STORE) ou nd_arg est null
              ((symbol? store) (node (replace-var store t))) ; val est un symbole (VAR_STORE)
              (else (node (replace-fn store t))))))))        ; val est un triplet (FN_STORE)    
                

(define node
  (lambda (store)
    (if (procedure? store) store ; if store is already a node
        (lambda nd_arg
          (cond ((null? nd_arg) store)                                        ; we want the store value
                ((list? (car nd_arg)) ((replace (car nd_arg)) (node store)))  ; we want to replace variables by their valuesµ
                (else ((car nd_arg) store)))))))                              ; we want to apply a function to the node


;-----------------------------------------------------------;
;                           TESTS                           ;
;-----------------------------------------------------------;


(define recover
  (lambda (nd)
    (let ((val nd))
      (val))))

(define inc
   (lambda (a)
     (+ a 1)))

((node 2) inc)     ;[[(node f)]]
(inc ((node 2)))   ;[[(f store)]]
((node 'x) '((y . 1) (x . 4)))

(define triplet (cons '+ (cons '((node 1) (node 2) (node 3)) (list inc))))
(define tripl (cons '+ (cons '((node 1) (node 2) (node 3)) (list (caddr triplet)))))
((caddr triplet) 4)
((caddr tripl) 4)

(equal? ((node 'x)) ((node 'x)))



;-----------------------------------------------------------;
;                          FACTORY                          ;
;-----------------------------------------------------------;

; FACTORY_ARG : représente soit un nombre, soit un symbole ou soit un NODE

; Prend en entrée 2 ou plus FACTORY_ARGs et retourne un NODE correspondant
; à l'opération d'addition.
(define Add
  (lambda a
    (sf-add (map node a))))

; Prend en entrée 2 ou plus FACTORY_ARGs et retourne un NODE correspondant
; à l'opération de multiplication.
(define Mult
  (lambda a
    (sf-mult (map node a))))

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

; Prend en entrée un NODE, si le STORE de ce NODE est un atom (soit CST_STORE
; soit un VAR_STORE), 'sf-aux' retourne ce même NODE. Si le STORE est un
; FN_STORE, retourne ce NODE simplifié si c'est possible.
(define sf-aux
  (lambda (node)
    (let ((store (node)))
      (if (atom? store) node                ; si le STORE est un CST_STORE ou un VAR_STORE
          ((caddr store) (cadr store))))))  ; si le STORE est un FN_STORE


; Prend en premier argument un CST_STORE ou un VAR_STORE et en deuxième argument un NODE
; et retourne 1 si les 2 arguments sont égaux (dans le sens de 'equal?'). 0 aussi non.
(define is-eq?
  (lambda (val)
    (lambda (node)
      (if (equal? val (node)) #t #f))))

; Prend en entrée un NODE et une valeur booléenne 'bool' et retourne une liste contenant uniquement
; le NODE si 'bool' est vrai, retourne une liste vide aussi non.
(define remove
  (lambda (node bool)
    (if bool '() (list node))))

; Prends en entrée une liste de NODEs 'nodes' et retourne une liste de NODEs dont le premier élément
; est la somme de toutes les NODEs de 'nodes' dont le STORE est un CST_STORE et ou les autres NODEs
; gardent leur ordre d'avant la simplification.
(define add-cst
          
(define sf-add
  (lambda args
    (let ((nodes1 (map sf-aux args)))
      (let ((nodes2 (add-cst nodes1)))                                         ; add the constants together
        (let ((nodes3 (append (map remove nodes2 (map (is-eq? 0) nodes2)))))   ; ('+ ((node a) (node 0)) sf-add) => ('+ node a)
          (let 
              ; ('+ ((node a) ('+ ((node b) (node c)) sf-add) sf-add) => ('+ ((node a) (node b) (node c)) sf-add)
              ; check if + 2 constantes qu'on peut regrouper
        
        
(define sf-mult
  (lambda (args)
    (args)))