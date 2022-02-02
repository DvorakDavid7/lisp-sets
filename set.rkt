;;;; Racket set library
;;;; Author David Dvorak

#lang racket

(provide make-set)
(provide union)
(provide intersect)
(provide complement)
(provide diff)



; =================== utils ===================

(define (append-element e l)
    (append l (cons e null))
)


(define (is-in? element l)
    (if (null? l)
        #f
        (if (= element (car l))
            #t
            (is-in? element (cdr l))
        )
    )
)


(define (remove-dup l)
    (remove-dup-impl l '())
)


(define (remove-dup-impl l acc)
    (if (null? l)
        acc
        (if (is-in? (car l) acc)
            (remove-dup-impl (cdr l) acc)
            (remove-dup-impl (cdr l) (append-element (car l) acc))
        )
    )
)



; =================== set operations ===================
; supported operations: union, intersect, complement, diff


; make set out of provided list l
(define (make-set l)
    (remove-dup l)
)


; union of two provided lists. Returns result as set
(define (union l1 l2)
    (make-set (append l1 l2))
)


; intersect of two provided lists. Returns result as set
(define (intersect l1 l2)
    (let (
        [longer 
            (if (< (length l1) (length l2)) l2 l1)]
        
        [shorter
            (if (< (length l1) (length l2)) l1 l2)]
        )
        (make-set (intersect-impl longer shorter '()))
    )
)


(define (intersect-impl longer shorter acc)
    (if (null? longer)
        acc
        (if (is-in? (car longer) shorter)
            (intersect-impl (cdr longer) shorter (append-element (car longer) acc))
            (intersect-impl (cdr longer) shorter acc)
        )
    )
)


; complement of l in universum u. Returns result as set
(define (complement l u)
    (make-set (complement-impl l u '()))
)


(define (complement-impl l u acc)
    (if (null? u)
        acc
        (if (is-in? (car u) l)
            (complement-impl l (cdr u) acc)
            (complement-impl l (cdr u) (append-element (car u) acc))
        )
    )
)


; diff of l1 - l2. Returns result as set
(define (diff l1 l2)
    (make-set 
        (intersect l1 (complement l2 (union l1 l2))))
)
