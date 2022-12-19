;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Simple Recursion|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; -------------------
;; Matthew Li
;; Racket - Recursion
;; Simple Recursion
;; -------------------

;; When recursion is applied, every argument is either
;;    * one step closer to the base case
;;    * unchanged

;; For example:
;; Let's write a function to add a consumed number to each number in a list

;; (adder n lon) adds the consumed number n to each element in a list of numbers
;; adder: Num (listof Num) -> (listof Num)
(define (adder n lon)
  (cond
    [(empty? lon) empty] ;; Our base case: When the list reaches empty,
                         ;; produce an empty which is added onto the end of the resulting list
    
    [else (cons (+ n (first lon))
                (adder n (rest lon)))])) ;; The bulk of the function:
                                         ;; Create a list, the first element being
                                         ;; (n + original first element), the rest
                                         ;; being (adder n lon) applied recursively
                                         ;; with (rest lon) being the new input

;; To quickly verify if our function works, we can use an example test:
(check-expect (adder 5 '(1 2 3 4 5)) '(6 7 8 9 10))


;; Another example:
;; Let's write a function that consumes a string and produces the same string in all CAPITAL LETTERS
;; Notice that Racket has a built in string-upcase function, but this is purely as a simple example
;; that demonstrates how simple recursion works using a wrapper function

;; (upcaser str) produces the consumed string in all capital letters
;; upcaser: Str -> Str
(define (upcaser str)
  (list->string (upcaser/list (string->list str)))) ;; This is our main function. Because we want the
                                                    ;; user to input specific parameters, it acts as a
                                                    ;; wrapper function which converts the input into
                                                    ;; something that our helper function can use

;; Helper function:
;; upcaser/list: (listof Char) -> (listof Char)
(define (upcaser/list loc)
  (cond
    [(empty? loc) empty] ;; Base case: When list is empty, produce empty
    [else (cons (char-upcase (first loc)) (upcaser/list (rest loc)))])) ;; Recursive call
;; Notice that our helper function acts similarly to the adder function we coded previously
;; This is because our helper does the bulk of the work, and is then called by the main function

;; Test case:
(check-expect (upcaser "hello!") "HELLO!")


;; A final example:
;; This example takes a look at simple recursion in nested lists

;; A ContactList is a (listof (list Str Nat))
;; Requires:
;;       * Each Str is unique
;;       * Each Nat is unique
;; Example ContactList:
(define sample-cl (list (list "Matthew" 6477270727)
                        (list "Bobby" 4161112013)
                        (list "James" 4379901342))) ;; Note: These are not real phone numbers

;; Let's write a function that searches phone numbers in a ContactList and produces the contact name

;; (cl-search: number cl) consumes a Nat and a ContactList and produces the name in the CL of that Nat
;; cl-search: Nat ContactList -> (anyof Str empty)
(define (cl-search number cl)
  (cond
    [(empty? cl) empty] ;; Base case: Number is not found, produce empty
    [(= number (second (first cl))) (first (first cl))] ;; Number is found, produce name
    [else (cl-search number (rest cl))])) ;; Recursive call if number is not found in the first item

;; Some test cases:
(check-expect (cl-search 4161112013 sample-cl) "Bobby")
(check-expect (cl-search 0000000000 sample-cl) empty)