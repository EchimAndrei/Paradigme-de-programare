#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2)
  (helper-longest w1 w2 '()))

(define (helper-longest w1 w2 acc)
  (let ((make-answer (lambda () (list (reverse acc) w1 w2))))
    (cond
      ((or (null? w1) (null? w2)) (make-answer))
      ((equal? (car w1) (car w2))
       (helper-longest (cdr w1) (cdr w2) (cons (car w1) acc)))
      (else (make-answer)))))


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection

(define (longest-common-prefix-of-collection words)
  (lcmpoc-helper (collection-rest words) (collection-first words)))

(define (lcmpoc-helper words current-prefix)
  (cond
    ((null? current-prefix) '())
    ((collection-empty? words) current-prefix)
    (else 
      (let ((new-prefix (longest-common-prefix current-prefix (collection-first words))))
        (lcmpoc-helper (collection-rest words) (collection-first new-prefix))))))


(define (match-pattern-with-label st pattern)
  (let* ((branch (get-ch-branch st (car pattern))))
  (cond
    ((equal? branch #f) (list #f '()))
    (else
     (let* ((label (get-branch-label branch))
         (subtree (get-branch-subtree branch))
         (lcp (longest-common-prefix label pattern)))
     (cond
       ((equal? (car lcp) pattern) #t) ;caz 1
       ((null? (second lcp)) (list label (third lcp) subtree)) ;caz 2
       (else (list #f (first lcp))) ;caz 3
       ))
     )
   ))
)


(define (st-has-pattern? st pattern)
  (let* ((match-result (match-pattern-with-label st pattern)))
    (cond
      ((equal? match-result #t) #t)
      ((and (list? match-result) (equal? (first match-result) #f)) #f)
      (else (st-has-pattern? (third match-result) (second match-result)))
  ))
 )


(define (get-suffixes text)
  (cond
    ((null? text) (empty-collection))
    (else (collection-cons text (get-suffixes (cdr text))))))

(define (get-ch-words words ch)
  (collection-filter (lambda (word)
            (and (not (null? word))
                 (equal? (car word) ch)))
         words))


(define (ast-func suffixes)
  (cons (list (car (collection-first suffixes))) (collection-map cdr suffixes)))


(define (cst-func suffixes)
  (let ((prefix (longest-common-prefix-of-collection suffixes)))
    (let ((new-suff (collection-map (lambda (suf) (drop suf (length prefix))) suffixes)))
      (cons prefix new-suff))))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (let* ((branches (collection-map (lambda (ch)
                         (let* ((suff-with-ch (get-ch-words suffixes ch)))
                           (cond ((collection-empty? suff-with-ch) '())
                                 (else (let* ((label-result (labeling-func suff-with-ch))
                                              (label (car label-result))
                                              (new-suff (cdr label-result)))
                                         (cons label (suffixes->st labeling-func new-suff alphabet)))))))
                       alphabet)))
    (collection-filter (lambda (branch) (not (null? branch))) branches)))

; nu uitați să convertiți alfabetul într-un flux

(define (list->stream L)
  (if (null? L)
      empty-stream
      (stream-cons (car L) (list->stream (cdr L)))))

(define (text->st labeling-func)
  (lambda (text)
    (let* ((new-text (append text '(#\$)))
           (suffixe (get-suffixes new-text))
           (alphabet (sort (remove-duplicates (collection-first suffixe)) char<?)))
      (suffixes->st labeling-func suffixe (list->stream alphabet)))))

(define text->ast (text->st ast-func))

(define text->cst (text->st cst-func))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (let* ((result (text->ast text))) 
    (st-has-pattern? result pattern)))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
    (let* ((st (text->cst text)))
        (let loop((st st) (result '()))
            (cond
                ((st-empty? st) #f)
                (else
                    (let* ((subtree (get-branch-subtree (first-branch st)))
                            (label (get-branch-label (first-branch st)))
                            (full-result (append result label))
                            (next-branch (lambda () (loop (other-branches st) result)))
                            (loop-subtree (lambda () (loop subtree full-result))))
                        (cond
                            ; nod frunza -> next ramura
                            ((st-empty? subtree) (loop (other-branches st) result))

                            ; e bun afisam ce avem
                            ((>= (length full-result) len) (take full-result len))

                            ; nu avem solutie pe ramura -> next ramura
                            ((equal? (loop subtree full-result) #f) (loop (other-branches st) result))

                            ;  verificam subarborele pentru rezolvare
                            (else (loop subtree full-result)))))))))