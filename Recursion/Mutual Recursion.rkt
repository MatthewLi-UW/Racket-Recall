;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Mutual Recursion|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; -------------------
;; Matthew Li
;; Racket - Recursion
;; Mutual Recursion
;; -------------------

;; In mutual recursion, functions recursively call each other
;; Let's take a look at an example of a pebbles game

;; Ex: Write a function of a game where whoever takes the last pebble wins
;; Matthew's strategy: Always take 1 pebble
;; Jeff's strategy: If pebble-count is even, take 2. If pebble-count is odd, take 1.

;; pebbles: Num -> Str
(define (pebbles n)
  (local
    [;; Matthew's strategy
     (define (matthew n) 
       (cond
         [(= 1 n) "Matthew wins"] ;; When there is 1 pebble remaining, Matthew wins
         [else (jeff (sub1 n))])) ;; Otherwise, Jeff goes with -1 pebble-count
     ;; Jeff's strategy
     (define (jeff n)
       (cond
         [(or (= 1 n) (= 2 n)) "Jeff wins"] ; When there is 1 or 2 pebbles remaining, Jeff wins
         [(even? n) (matthew (- n 2))] ;; If pebble-count is even, Matthew goes with -2 pebble-count
         [(odd? n) (matthew (sub1 n))]))] ;; If pebble-count is odd, Matthew goes with -1 pebble-count
    (matthew n)))

;; Note that in this case, Jeff always wins!
