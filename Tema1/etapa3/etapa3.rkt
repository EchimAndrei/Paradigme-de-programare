#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.

; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
  (let* ((result (text->cst text))) 
    (st-has-pattern? result pattern)))

; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim
; marcajul de final $ pentru a nu crește artificial lungimea
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).

;match-pattern-with-label returneaza asta:
;; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)

(define (longest-common-substring text1 text2)
    (let* ((st1 (text->cst text1))
            (suff2 (get-suffixes text2)))
        (let loop((st st1) (suff2 suff2) (result '()))
            (cond
                ((null? suff2) result)
                (else
                    (let* ((suffix (car suff2))
                            (match (match-pattern-with-label st suffix))
                            (rest-suffix (cdr suff2)))
                        (cond

                            ; se gaseste integral
                            ((equal? match #t)
                                    (loop st rest-suffix (if (> (length suffix) (length result)) suffix result)))

                            ((and (equal? match #f) (null? (second match)))
                                    (loop st rest-suffix result))

                            ((and (list? match) (equal? (first match) #f))
                                    (loop st rest-suffix (if (> (length (second match)) (length result)) (second match) result)))

                            ; dacă șablonul se potrivește cu eticheta dar nu este conținut în ea 
                            ; (ex: ("na", "na$", subarborele de sub eticheta "na") pentru șablonul inițial "nana$" și eticheta "na")
                            ; trebuie cautat mai departe in subarbore
                            ; si trebuie salvat in rezultat partea partiala gasita pana acum
                            (else (let* ((label (first match))
                                        (new-pattern (second match))
                                        (subtree (third match))
                                        (helper (longest-common-substring-helper subtree new-pattern '()))
                                        (label-full (append label new-pattern)))

                                    (cond
                                        ((equal? helper #t) (if (> (length label-full) (length result))
                                                                (loop st rest-suffix label-full)
                                                                (loop st rest-suffix result)))
                                        ; verificam daca second helper + ce am gasit deja > result
                                        (else
                                        (if (> (length (append label (second helper))) (length result))
                                                    (loop st rest-suffix (append label (second helper)))
                                                    (loop st rest-suffix result)))))))))))))

(define (longest-common-substring-helper st pattern result)
    (let* ((match-result (match-pattern-with-label st pattern)))
        (cond
        ((equal? match-result #t) #t)
        ; second = ce am gasit pana acum
        ((and (list? match-result) (equal? (first match-result) #f)) (list #f (append result (second match-result))))
        ; third = unde caut, second = ce caut, first = ce am gasit
        (else (longest-common-substring-helper (third match-result) (second match-result) (append result (first match-result)))))))
; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.
(define (repeated-substring-of-given-length text len)
    (let* ((st (text->cst text)))
        (let loop((st st) (result '()))
            (cond
                ((st-empty? st) #f)
                (else
                    (let* ((subtree (get-branch-subtree (first-branch st)))
                            (label (get-branch-label (first-branch st)))
                            (full-result (append result label))
                            (next-branch (lambda () (loop (other-branches st) result))))
                        (cond
                            ; nod frunza -> next ramura
                            ((null? subtree) (next-branch))

                            ; e bun!!!
                            ((>= (length full-result) len) (take full-result len))

                            ; nu avem solutie pe ramura -> next ramura
                            ((equal? (loop subtree full-result) #f) (next-branch))

                            ;  verificam subarborele pentru rezolvare
                            (else (loop subtree full-result)))))))))