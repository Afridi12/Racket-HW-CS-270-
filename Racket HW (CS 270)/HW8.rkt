#lang racket
(require rackunit)
(require rackunit/text-ui)

;CS 270
;Homework 8
;Professor B. Char, M. Boady,  J. Johnson, and G. Long

;Important Rules:
;1.) You may not use loop constructs like while/for/sum. If used, your answer will get a zero.
;2.) If the instructions state something must be recursive, you will recieve a zero if it is not recursive.
;    Recursive helper functions are allowed (the main function not being recursive).
;3.) You may not use the set! command. If used, your answer will get a zero.
;4.) Using If/Cond to explicitly pass tests instead of following the instructions
;    will always result in a zero for that question.

;Each of the below questions has two parts.
;First, you will be asked to write a Racket function to solve a problem.
;Secondly, you will be asked to prove by induction that your
;Racket code has some property.


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 1a (10 points)
;Write a recursive function to compute
;the sum( 6*x^2 , x = 1..n) for a given n
;You must write a recursive function.
;If you use any iterative commands (for/loop/sum/etc you will receive a 0)

; Computes sum( 6*x^2 , x = 1..n)
; Input:  n an integer >= 1
; Output: an integer, the result of the summation
(define (spec_sum n)
  (if (= n 1)
      6
      (+ (* 6 (* n n)) (spec_sum (- n 1)))
      )
  
)

;Test Bed
(display "Question 1a spec_sum Tests (5 points)\n")
(define-test-suite test_spec_sum
  (check-equal? (spec_sum 1) 6)
  (check-equal? (spec_sum 2) 30)
  (check-equal? (spec_sum 3) 84)
  (check-equal? (spec_sum 4) 180)
  (check-equal? (spec_sum 5) 330)
  (check-equal? (spec_sum 6) 546)
  (check-equal? (spec_sum 7) 840)
  (check-equal? (spec_sum 8) 1224)
  (check-equal? (spec_sum 9) 1710)
  (check-equal? (spec_sum 10) 2310)
)
(define q1a_score (- 10 (run-tests test_spec_sum 'verbose)))

;Question 1b (10 points)
;Prove by induction that
;for all integers n >= 1 -> (spec_sum n) = 2n^3+3n^2+n

;Give your proof below in comments
; Base Case:
; 1. (spec_sum 1)  Call Function on 1
; 2. (if (= 1 1) 6 ....)   Apply Func. Def
; 3. (if #t 6...)       Eval Equals
; 4. 6             Eval if

;Inductive Hypothesis:
; Assume n >= 1:
; (spec_sum n) = 2n^3+3n^2+n

; Inductive Case
; 1. (spec_num (+ w 1))                         ``````````````Call Function on 2
; 2. (if (= (+ w 1) 1) 6 ...)                    Apply Func. Def
; 3. (if #f 6 ...)                                         Eval Equals
; 4. (+ (* 6 (* (+ w 1) (+ w 1))) (spec_sum ( - (+ w 1) 1)))   Eval If
; 5. (+ (* 6 (+ w 1)^2) (spec_sum (- (+ w 1) 1)))              Simplify    
; 6. (+ (* 6 (+ w 1)^2) (spec_sum w))                          Simplify Algebraically
; 7. (+ (* 6 (+ w 1)^2) (+ (+ (* 2 (* w (* w w))) (* 3 (sqr w))) w)  Use IH
; 8. (6(w+1)^2) + (2w^3 + 3w^2 +w)                             Rewrite Algebraically
;9) (2w^3+3w^2+n)                          Factor



;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 2 (10 points)
; Write a recursive function evenzeros to check if a list of integers
; contains an even number of zeros.
; Don't forget the base case and the necessary recursion. 

; Check if a list contains an even number of zeros
; Input:  L is a list of integers.
; Output: a boolean value which is true when an even number of the elements
;          in L is equal to zero and false otherwise.
; 0 is even, so the Null list should return true
(define (evenzeros X)
  (cond
    [(null? X) #t]
    [(equal? '0 (first X))  (not (evenzeros (rest X)))]
    [else (evenzeros (rest X))]
    )
      
)
;Test Bed
(display "Question 2a evenzeros Tests (10 points)\n")
(define-test-suite test_even_zeros
  (check-equal? (evenzeros '()) #t)
  (check-equal? (evenzeros '(1)) #t)
  (check-equal? (evenzeros '(0)) #f)
  (check-equal? (evenzeros '(0 0)) #t)
  (check-equal? (evenzeros '(7 0)) #f)
  (check-equal? (evenzeros '(1 -2)) #t)
  (check-equal? (evenzeros '(0 0 1)) #t)
  (check-equal? (evenzeros '(4 0 1)) #f)
  (check-equal? (evenzeros '(1 0 8)) #f)
  (check-equal? (evenzeros '(0 11 0 -9)) #t)
)
(define q2a_score (- 10 (run-tests test_even_zeros 'verbose)))
;Question 2b (10 points)
;Prove by induction, algebra, and equational reasoning that
;If L contains an even number of zeros then (equal? (evenzeros L) #t)
;If L contains an odd number of zeros then (equal? (evenzeros L) #f)
;Hint: You need 4 cases (cons 0 E), (cons x E), (cons 0 O), (cons x O)
;Where, x!=0, E is a list with an even number of zeros and O is a list
;with an odd number of zeros.

;Base Case:
; 1. (evenzeros '())
; 2. (cond [(null? '()) #t]....)
; 3. (cond [#t #t] ....)
; 4. #t

;Inductive Hypothesis: 
;If L contains an even number of zeros then (equal? (evenzeros L) #t)
;If L contains an odd number of zeros then (equal? (evenzeros L) #f)

;Inductive Proof:
; Case 1:

; 1. (evenzeros (cons 0 E))  Call function on 0
; 2. (cond [(null? (cons 0 E) #t] ...) Apply func Def
; 3. (cond [#f #t]...) Eval Null
; 4. (cond [(equal? '0 (first (cons 0 E))) (not(evenzeros (rest (cons 0 E))))]...)   Move to Next Cond.
; 5. (cond [#t (not (evenzeros(rest (cons 0 E))))]...) Eval equals
; 6. (not(evenzeros(rest(cons 0 E))))              Evaluate Cond
; 7. (not(evenzeros E))            Rest and Cons Cancel
; 8. (not #t)                      Use IH
; 9. #f                        Evaluate Not

;Case 2:

;1) (evenzeros (cons x E))    Call function on x
;2) (cond [(null? (cons x E) #t] ...) Apply func Def
;3) (cond [#f #t]...) Eval Null
; 4. (cond [(equal? '0 (first (cons x E))) (not(evenzeros (rest (cons x E))))]...)   Move to Next Cond.
;5) (cond [(#f (not zeros(rest(cons x E))))]...)        Eval Equals
;6) (cond [else (evenzeros (rest (cons x E))))])    Move to next Cond.
;7) (evenzeros (rest(cons x E)))         Eval Cond
;8) (evenzeros E)               Rest and Cons Cancel out
;9) #t                 Use IH

;Case 3:
; 1. (evenzeros (cons 0 O))  Call function on 0
; 2. (cond [(null? (cons 0 O) #t] ...) Apply func Def
; 3. (cond [#f #t]...) Eval Null
; 4. (cond [(equal? '0 (first (cons 0 O))) (not(evenzeros (rest (cons 0 O))))]...)   Move to Next Cond.
; 5. (cond [#t (not (evenzeros(rest (cons 0 O))))]...) Eval equals
; 6. (not(evenzeros(rest(cons 0 O))))              Evaluate Cond
; 7. (not(evenzeros O))            Rest and Cons Cancel
; 8. (not #f)                      Use IH
; 9. #t                        Evaluate Not

;Case 4:

;1) (evenzeros (cons x O))    Call function on x
;2) (cond [(null? (cons x O) #t] ...) Apply func Def
;3) (cond [#f #t]...) Eval Null
; 4. (cond [(equal? '0 (first (cons x O))) (not(evenzeros (rest (cons x O))))]...)   Move to Next Cond.
;5) (cond [(#f (not zeros(rest(cons x O))))]...)        Eval Equals
;6) (cond [else (evenzeros (rest (cons x O))))])    Move to next Cond.
;7) (evenzeros (rest(cons x O)))         Eval Cond
;8) (evenzeros O)               Rest and Cons Cancel out
;9) #f                 Use IH




; 
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Q3a (10 Points)
;Write a recursive function duplicate that takes every element in a list
;and makes a second copy of the item.
;For example if we started with (1 2 3)
;then the duplicated list would be (1 1 2 2 3 3)


; Duplicates Elements in a list
; Input:  X a list
; Output: A new list with two copies of even value in X
(define (duplicate X)
 (if (equal? X '())
      '()
      (cons (first X) (cons (first X) (duplicate (rest X))))
)
)
(display "Question 3a duplicate Tests (10 points)\n")
(define-test-suite test_duplicate
  (check-equal? (duplicate '()) '())
  (check-equal? (duplicate '(1)) '(1 1))
  (check-equal? (duplicate '(1 2)) '(1 1 2 2))
  (check-equal? (duplicate '(4 6)) '(4 4 6 6))
  (check-equal? (duplicate '((1) (2 3))) '((1) (1) (2 3) (2 3)))
  (check-equal? (duplicate '(4 5 6)) '(4 4 5 5 6 6))
  (check-equal? (duplicate '(7 8 9 10)) '(7 7 8 8 9 9 10 10))
  (check-equal? (duplicate '(1 2 3 4 5)) '(1 1 2 2 3 3 4 4 5 5))
  (check-equal? (duplicate '(9 9 9)) '(9 9 9 9 9 9))
  (check-equal? (duplicate '(1 4 5 6 4 3 4 5))
                '(1 1 4 4 5 5 6 6 4 4 3 3 4 4 5 5))
)
(define q3a_score (- 10 (run-tests test_duplicate 'verbose)))

;Q3b (10 Points)
;Prove By Induction
;(length (duplicate L)) = (* 2 (length L))
;You may use the following properties of length
;Length Property 1: (length '()) = 0 
;Length Property 2: If a is an object and B is a list
;(length (cons a B)) = (+ 1 (length B))
;You may Justify lines by saying [By Length Property 1]
;Hint: An equals can be used both ways.

;Base Case:
; 1. (length(duplicate '()))        Call Function on '()
; 1. (length(if (equal? '() '()) '()...))    Apply Function Definition
; 2. (length(if #t '()...))           Evaluate Equals
; 4. (length'())                  Evaluate If
; 5. 0                             By Length Property 1

;Inductive Hypothesis:
;(length (duplicate L)) = (* 2 (length L))

;Inductive Case:
;1. (length(duplicate (cons a L)))     Call function on Larger List
;2. (length(if (equal? (cons a L) '()) '()...))      Apply Function Def.
;3. (length(if #f '()...))       Eval Equals
;4. (length(cons (first (cons a L)) (cons (first (cons a L)) (duplicate (rest (cons a L))))))       Eval If
;5. (length(cons a (cons a (duplicate (rest (cons a L))))))     First and Cons Cancel
;6. (length(cons a (cons a (duplicate L))))     Rest and Cons Cancel out
;7. (+1 (length(cons a (duplicate L))))        Apply Length Property 2
;8. (+1 (+1 (length(duplicate L))))        Apply Length Property 2
;9. (+1 (+1 (*2 (length L))))     Use IH
;10. (*2 (+1 (length L)))                  Rewrite Algebraically 



;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 4a (10pts)
;Write a recursive function (cut_end L) that removes the last element from the list

; Removes the last element in a list
; Input:  X non-empty a list
; Output: A new list with the last element removed
(define (cut_end L)
  (if (null? L)
      '()
      (if (null? (rest L))
          '()
          (cons (first L) (cut_end (rest L)))
          )
       
  )
)

(display "Question 4a cut_end Tests (10 points)\n")
(define-test-suite test_cut_end
  (check-equal? (cut_end '(1)) '())
  (check-equal? (cut_end '(1 2)) '(1))
  (check-equal? (cut_end '(3 4 5)) '(3 4))
  (check-equal? (cut_end '( (1) (2) (3) )) '( (1) (2) ))
  (check-equal? (cut_end '((1 2 3 4))) '())
  (check-equal? (cut_end '((1 2) (3 4))) '((1 2)))
  (check-equal? (cut_end '(9 9 8)) '(9 9))
  (check-equal? (cut_end '(AND A B)) '(AND A))
  (check-equal? (cut_end '(NOT X)) '(NOT))
)
(define q4a_score (- 10 (run-tests test_cut_end 'verbose)))

;Question 4b
;Prove by Induction that (length (cut_end L)) = (- (length L) 1)
;for all L where (length L) >= 1
;You may use the properties of length from Question 3

;Base Case:
;1) (length(cut_end '()))        Call function on '()
;2) (length(if (null? '()) '()....))   Apply function definition
;3) (length(if #t '() ....))            Eval Null?
;4) (length '())                Eval If
;5) 0                         By Length Property 1

;Inductive Hypothesis
; Assume : (length L) >= 1
;(length (cut_end L)) = (- (length L) 1)

;Inductive Case:
;1) (length(cut_end (cons a L)))     Call Function on Longer List
;2) (length(if(null? (cons a L))'()...))    Apply Function Def.
;3) (length(if #f '()....))      Eval Null
;4) (length(if (null? (rest (cons a L))) '()...))      Eval If
;5) (length(if #f '()...))    Eval Null
;6) (length(cons(first(cons a L)) (cut_end (rest(cons a L)))))   Eval If
;7) (length(cons a (cut_end (rest(cons a L))))) First and Cons Cancel out
;8) (length(cons a (cut_end L)))   Rest and Cons Cancel out
;9) (+1 (length (cons a L)))       By Length Property 2
;10) (+1 (- (length L) 1))         Use IH
;11) (length L)                    Evalate Addition





;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 5a (10pts)
;Write a recursive function (add_pairs L)
;that adds pairs of numbers.
;You may assume the length of L will always be even.

; Adds pairs of numbers
; Input:  L a list (the list must have even length)
; Output: A new list with pairs of elements added together.
(define (add_pairs L)
  (if (null? L)
      '()
      (cons (+ (first L) (second L)) (add_pairs (rest (rest L))))
       
  )
)

(display "Question 5a add_pairs Tests (10 points)\n")
(define-test-suite test_add_pairs
  (check-equal? (add_pairs '()) '())
  (check-equal? (add_pairs '(1 2)) '(3))
  (check-equal? (add_pairs '(1 2 3 4)) '(3 7))
  (check-equal? (add_pairs '(2 2 2 2)) '(4 4))
  (check-equal? (add_pairs '(0 -1 -2 3)) '(-1 1))
  (check-equal? (add_pairs '(1 1 1 1)) '(2 2))
  (check-equal? (add_pairs '(1 2 3 4 5 6 7 8)) '(3 7 11 15))
  (check-equal? (add_pairs '(9 9 9 9 9 9)) '(18 18 18))
  (check-equal? (add_pairs '(7 3 4 6 5 5)) '(10 10 10))
  (check-equal? (add_pairs '(-9 9 -8 8)) '(0 0))
  
)
(define q5a_score (- 10 (run-tests test_add_pairs 'verbose)))

;Question 5b
;Prove by Induction that (length (add_pairs L)) = 1/2*(length L)
;for all L where (even? (length L)) and (length L) >= 0
;You may use the properties of length from Question 3

;Base Case:
;1) (length (add_pairs (cons a '())))    Call function on L
;2) (length (if (null? (cons a L) '() ...))     Apply Function Def.
;3) (length (if #f '() ....)) Eval Null
;4) (length '()) Eval If
;5) 0     By Length Property 1

;Inductive Hypothesis
;Assume : Ax in L (even? (length L)) and (length L) >= 0
;(length (add_pairs L)) = 1/2*(length L)

;Inductive Proof:
;1) (length (add_pairs (cons a (cons b L))))     Call Function on longer List
;2) (length (if (null? (cons a (cons b L))) '() ...) APply function def
;3) (length (if #f '()...))     Eval Null
;4) (length (cons (+ (first (cons a (cons b L))) (second (cons a (cons b L)))) (add_pairs (rest (rest (cons a (cons b L))))))  Eval If
;5) (length (cons (+ a (second (cons a (cons b L))) (add_pairs (rest (rest (cons a (cons b L)))))  First and Conc cancel out
;6) (length (cons (+ a b (add_pairs (rest (rest (cons a (cons b L))))))))  Second and Cons Cancel out 
;7) (length (cons (+ a b (add_pairs (rest (cons b L))))))       Rest and Cons Cancel
;8) (length (cons (+ a b (add_pairs L))))     Rest and Cons Cancel Out
;9) (+1 (length (add_pairs L)))           By Length Property 2
;10) (+1 (* 1/2 (length L))           Use IH



;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;;;;;;;;;;;;;;Grade Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Grade Summary------\n")
(display "Q1a Scored: ")
(display q1a_score)
(display "/10\n")
(display "Q1b Scored: ?/10 (Graded by TA)\n")
(display "Q2a Scored: ")
(display q2a_score)
(display "/10\n")
(display "Q2b Scored: ?/10 (Graded by TA)\n")
(display "Q3a Scored: ")
(display q3a_score)
(display "/10\n")
(display "Q3b Scored: ?/10 (Graded by TA)\n")
(display "Q4a Scored: ")
(display q4a_score)
(display "/10\n")
(display "Q4b Scored: ?/10 (Graded by TA)\n")
(display "Q5a Scored: ")
(display q5a_score)
(display "/10\n")
(display "Q5b Scored: ?/10 (Graded by TA)\n")


(define grand_total (+ q1a_score q2a_score q3a_score q4a_score q5a_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")