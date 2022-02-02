#lang racket

(require "set.rkt")
(require rackunit)

; make set
(check-equal? 
    (make-set '(1 1 2 2 3 3))
    '(1 2 3)
)

; union
(check-equal? 
    (union '(1 2 3) '(2 3 5 6)) 
    '(1 2 3 5 6)
)


; intersect
(check-equal?
    (intersect '(1 2 3) '( 1 2 5))
    '(1 2)
)

(check-equal?
    (intersect '(6 7 8) '( 1 2 5))
    '()
)

(check-equal?
    (intersect '(3 5 1) '())
    '()
)

; complement
(check-equal?
    (complement '(2 3 4) '(0 1 2 3 4 5 6 7 8 9))
    '(0 1 5 6 7 8 9)
)

; diff
(check-equal?
    (diff '(1 2 3 4) '(2 3))
    '(1 4)
)