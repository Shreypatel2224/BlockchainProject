#lang racket

;; A [Hash-Table-of X Y] is a (make-hash (list (list X Y) ...))
;; where X is the key and Y is the value that the key maps to

;; make-hash : [List-of [List-of-Two X Y]] -> [Hash-table-of X Y]
;; Creates a hash table from a list of pairs
(define (make-hash associations)
  (make-immutable-hash
   (map
    (λ (p)
      (if (and (list? p) (= (length p) 2))
          (cons (car p) (cadr p))
          (raise-argument-error 'make-hash "list-of-two?" p)))
    associations)))

;; hash-update : [Hash-table-of X Y] X (Y -> Y) Y
;; updates entry using function if present, else default
(define (hash-update h k upd def)
  (hash-set h k (if (hash-has-key? h k) (upd (hash-ref h k)) def)))


;; NOTE: these below are defined in the Racket library

;; hash-has-key? : [Hash-table-of X Y] X -> Boolean
;; Checks if a hash table has a key. Returns #true if it does, #false otherwise

;; hash-ref : [Hash-table-of X Y] X -> Y
;; Returns the value associated with the key in the hash table

;; hash-set : [Hash-table-of X Y] X Y -> [Hash-table-of X Y]
;; Returns a new hash table with the key mapped to the value

;; hash-remove : [Hash-table-of X Y] X -> [Hash-table-of X Y]
;; Returns a new hash table with the key removed

;; hash-keys : [Hash-table-of X Y] -> [List-of X]
;; Returns a list of all the keys in the hash table

;; hash-values : [Hash-table-of X Y] -> [List-of Y]
;; Returns a list of all the values in the hash table



(provide make-hash hash-has-key? hash-ref hash-set hash-remove hash-keys hash-values)
