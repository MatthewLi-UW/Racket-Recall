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
;; Alice's strategy: Always take 1 pebble
;; Bob's strategy: If pebble-count is even, take 2. If pebble-count is odd, take 1.

;; pebbles: Num -> Str
(define (pebbles n)
  (local
    [;; Alice's strategy
     (define (alice n) 
       (cond
         [(= 1 n) "Alice wins"] ;; When there is 1 pebble remaining, Alice wins
         [else (bob (sub1 n))])) ;; Otherwise, Bob goes with -1 pebble-count
     ;; Bob's strategy
     (define (bob n)
       (cond
         [(or (= 1 n) (= 2 n)) "Bob wins"] ; When there is 1 or 2 pebbles remaining, Bob wins
         [(even? n) (alice (- n 2))] ;; If pebble-count is even, Alice goes with -2 pebble-count
         [(odd? n) (alice (sub1 n))]))] ;; If pebble-count is odd, Alice goes with -1 pebble-count
    (alice n)))

;; Note that in this case, Bob always wins!