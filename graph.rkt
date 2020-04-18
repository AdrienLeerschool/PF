#lang racket
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
; de 'nodes' ajoutées à 'var' (dans le même ordre que celui de la liste 'nodes' initialement donnée).
; First call : (re-order nodes '() '())
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
     (re-order (map (replace t) (cadr tri)) '() '())))
                    

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
                (else ((car nd_arg) store)))))))                              ; we want to apply a function to the node's store

(define nd (node 4))
(nd)
(add1 (nd))




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
  (lambda (nd)
    (let ((store (nd)))
      (if (atom? store) nd                  ; si le STORE est un CST_STORE ou un VAR_STORE
          ((caddr store) (cadr store))))))  ; si le STORE est un FN_STORE


; Prend en premier argument un CST_STORE ou un VAR_STORE et en deuxième argument un NODE
; et retourne 1 si les 2 arguments sont égaux (dans le sens de 'equal?'). 0 aussi non.
(define is-eq?
  (lambda (val)
    (lambda (nd)
      (if (equal? val (nd)) #t #f))))

; Prend en entrée un NODE et une valeur booléenne 'bool' et retourne une liste contenant uniquement
; le NODE si 'bool' est vrai, retourne une liste vide aussi non.
(define remove
  (lambda (nd bool)
    (if bool '() (list nd))))

; Prends en entrée une liste de NODEs 'nodes' et un compteur 'cnt' (initialisé à 0 au premier appel)
; et retourne une liste de NODEs dont le premier élément est la somme de toutes les NODEs de 'nodes'
; dont le STORE est un CST_STORE + cnt et ou les autres NODEs gardent leur ordre d'avant la simplification. 
; Si la somme des constantes vaut zero et qu'il reste d'autres NODEs dans la liste, supprime le NODE
; contenant le CST_STORE nul, retourne une node dont le STORE vaut 0 aussi non.
(define add-cst
  (lambda (args cnt)
    (if (null? args) (list (node cnt))
        (let ((nxt (car args)))
          (if (number? (nxt)) (add-cst (cdr args) (+ cnt (nxt)))
              (if (= 0 cnt) args
                  (cons (node cnt) args)))))))
    
        
             
(define sf-add
  (lambda (args)
    (let ((nodes (add-cst (map sf-aux args) 0)))
      (if (= 1 (length nodes)) (car nodes)
          nodes))))
        
        
(define sf-mult
  (lambda (args)
    (args)))

;-----------------------------------------------------------;
;                         AFFICHAGE                         ;
;-----------------------------------------------------------;

(define get-stores
  (lambda (nodes)
    (if (null? nodes) '()
        (cons ((car nodes)) (get-stores (cdr nodes))))))

(define repr
  (lambda (store)
    (if (list? store)
        (append (list (car store)) (map repr (get-stores (cadr store))))
        store)))
    
;-----------------------------------------------------------;
;                           TESTS                           ;
;-----------------------------------------------------------;

(define disp
  (lambda (nodes vals)
    (if (not (list? nodes)) (nodes)
        (if (null? nodes) (reverse vals)
            (disp (cdr nodes) (cons ((car nodes)) vals))))))


(define ls (list '+ (list (node (list '+ (list (node 'x) (node 'y)) sf-add)) (node 'z)) sf-add))
(define nodes (node ls))
(nodes repr)

