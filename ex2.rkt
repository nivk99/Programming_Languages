#lang pl



#|
*@brief This is the second task in a programming language
*@date 2022-12-15
|#


#|

_____________________________________________Q(1 A)_____________________________________________________________________


#|
* @brief - This is the writings of BNF
 This is a question that took me 60 minutes
  This is a boring question
|#



 
 
 Defining 4 new types:
   
    1.SDC =single number character:
    (define-type SDC = (U #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

    2.𝜆 =specify the empty string:
    (define-type 𝜆 ="")

    4. SWS= A string with a space:
   (define-type SWS = (Stringof " "))

   3. SDN-List = A String of single number character:
   (define-type D = (Stringof number))


The  BNF:
   
   <SE>::= <NUM> (1)
          |<SDC> (2)
          |<STR> (3)
   
   
   <STR>::= <D> (4)
           |<𝜆>  (5)
           |<LSWS> (6)
           |{string-append <STR> <STR>} (7)
           |{string <strof>} (8)
           |{number->string <NUM> } (9)
           |{string-insert <STR> <SDC> <num>} (10)
   
   <STRof>::=   <SDC> (11)
               |{<SDC> <STRof>} (12)
   
   <NUM>:: <Number> (13)
          |{string-length <STR>} (14)

  
  <Number>:= Any number in the racket (15)
   
 
   <LSWS>::= <SWS> (16)
             |{<SWS> <LSWS>} (17)

   
   <D>::=  A String of Number (18)
   
   <𝜆>::= "" (19)
   
   <SWS>::=" " (20)
   
   <SDC>::   #\0 (21)
           | #\1 (22)
           | #\2 (23)
           | #\3 (24)
           | #\4 (25)
           | #\5 (26)
           | #\6 (27)
           | #\7 (28)
           | #\8 (29)
           | #\9 (30)
   



_____________________________________________ END Q(1 A)_____________________________________________________________________


_____________________________________________ Q(1 B)_____________________________________________________________________


#|
* @brief - This is an example of BNF writing
 This is a question that took me 30 minutes
  This is a boring question
|#



1)  ( string-append ( string #\1 #\2 #\4 ) "12" )



                                                      <SE>
                                                       |
                                                      <STR> (3)
                                                        |
                                                    {string-append   <STR>     <D>}(7)
                                                                       |        |
                                                           {string <STRof>}(8) "12"(18)
                                                                      |
                                                                {<SDC> <STRof>}(12)
                                                                    |      |
                                                                 #\1(23){<SDC> <STRof>}(12) 
                                                                            |       |
                                                                        #\2(22)   <SDC>(11)
                                                                                     |
                                                                                   #\4(25)


2)( number->string ( string-length "0033344" ) )


                                                               <SE>
                                                                 |
                                                               <STR>(3)
                                                                 |
                                                       {number->string <NUM> }(9)
                                                                         |
                                                                  {string-length <STR>} (14)
                                                                                    |
                                                                                    <D>(4)
                                                                                     |
                                                                                  "0033344" (18)
                                                                      



3) ( number->string 156879 ) 

                                                               <SE>
                                                                 |
                                                               <STR> (3)
                                                                 |
                                                      {number->string <NUM> } (9)
                                                                        |
                                                                    <Number> (13)
                                                                       |
                                                                       156879 (15)



_____________________________________________ END Q(1 B)_____________________________________________________________________
|#



#|
_____________________________________________ Q(2)_____________________________________________________________________
|#

#|
* @brief - quares function which takes a list of numbers as input, and produces a number which is the sum of the squares of all of the numbers in the list.
  It took me half an hour to write
  That's an okay question
|#

(: square : Number -> Number) 
(define (square num)
  (* num num))


(test (square 0) => 0)
(test (square -2) => 4)
(test (square -5) => 25)
(test (square 5) => 25)


(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
  (foldl + 0 (map square lst)))

#|TESTS Q(2)|#
(test (sum-of-squares '(1 2 3)) => 14) 
(test (sum-of-squares '(3)) => 9)
(test (sum-of-squares '(1 2)) => 5)
(test (sum-of-squares '(0 2)) => 4)
(test (sum-of-squares '(0 -2)) => 4)
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(-1)) => 1)
(test (sum-of-squares '(0)) => 0)

#|
_____________________________________________  END Q(2)_____________________________________________________________________
|#



#|
_____________________________________________ Q(3 A)_____________________________________________________________________
|#

#|
* @brief - It is a function that calculates a polynomial according to the coefficients and according to x
* It's a function that took me an hour. That's a nice question
|#

(: createPolynomial : (Listof Number) -> (Number -> Number)) 
(define (createPolynomial coeffs)
  
  (: poly : (Listof Number) Number Integer Number -> Number) 
  (define (poly list x power accum) 
     (if (null? list) accum
      (poly (rest list) x (+ power 1)(+ accum (* (first list)(expt x power))))))  
  (: polyX : Number -> Number) 
  (define (polyX x)
    (poly coeffs x 0 0))
  polyX)

#|TESTS Q(3 A)|#
(test ((createPolynomial '()) 5) => 0)
(test ((createPolynomial '(2 3 4 5)) 0) => 2)
(test ((createPolynomial  '(3 2 10)) 4) => 171)
(test ((createPolynomial '(6)) 2 ) => 6)
(test ((createPolynomial '(5 3 6 7)) -5) => -735)

#|
_____________________________________________ END Q(3 A)_____________________________________________________________________
|#

#|
_____________________________________________ Q(3 B1)_____________________________________________________________________
|#



#|
* @brief - This is BNF writing
  take me 30 min.
  very nice quation
|#




#|
 
  The grammar: 
    <PLANG> ::={{poly <Aes>} {<Aes>}} 


  <AEs>   ::= <AE>
              |{<AE> <Aes>}
  <AE> ::= <num>
  | { + <AE> <AE> }
  | { - <AE> <AE> }
  | { * <AE> <AE> }
  | { / <AE> <AE> }

  
  |# 




#|
_____________________________________________ END Q(3 B1)_____________________________________________________________________
|#

#|
_____________________________________________ Q(3 B2)_____________________________________________________________________
|#


#|
* @brief - the parser for the new language.
  It took me an 5 hours.
  that's a good question
|#

(define-type PLANG 
    [Poly (Listof AE) (Listof AE)]) 
 
  (define-type AE 
    [Num  Number] 
    [Add  AE AE] 
    [Sub  AE AE] 
    [Mul  AE AE] 
    [Div  AE AE]) 
 


  (: sexpr : (U (Listof Any) Any)  -> AE) 
  ;; to convert s-expressions into AEs 
(define (sexpr sxp)
        (cond
               [(number? sxp) (Num sxp)]
              [(and (list? sxp)
                    (= (length sxp) 3)
                    (eq? (first sxp) '+))
               (Add (sexpr (first(rest sxp)))
                    (sexpr (first(rest (rest sxp)))))]
              [(and (list? sxp)
                    (= (length sxp) 3)
                    (eq? (first sxp) '-))
               (Sub (sexpr (first(rest sxp)))
                    (sexpr (first(rest (rest sxp)))))]
                [(and (list? sxp)
                    (= (length sxp) 3)
                    (eq? (first sxp) '*))
               (Mul (sexpr (first(rest sxp)))
                    (sexpr (first(rest (rest sxp)))))]
               [(and (list? sxp) 
                    (= (length sxp) 3)
                    (eq? (first sxp) '/))
               (Div (sexpr (first(rest sxp)))
                    (sexpr (first(rest (rest sxp)))))]
              
              [else (error 'sexpr "bad syntax2 in ~s" sxp)]
         ))
 



(: list->poly-list : (Listof Any) -> (Listof AE))
(define (list->poly-list lst)
  (cond [(null? lst) null]
        [(number? (first lst)) (cons (Num (first lst)) (list->poly-list (rest lst)))]
        [(and (list? (first lst))
        (= (length (first lst)) 3)) (cons (sexpr (first lst)) (list->poly-list (rest lst)))]
        [else (error 'parse-sexpr "bad syntax1 in ~s" (first lst))]))

  (: pars-sexpr : Sexpr -> PLANG)
   (define (pars-sexpr sexpr) 
    (match sexpr
     [(list (list 'poly (and x (or number:x)) ... ) (list (and y (or number:y)) ... ))
      (cond [(null? x) (error 'pars-sexpr: "parse: at least one coefficient is required in ~s" sexpr)]
            [(null? y) (error 'pars-sexpr: "parse: at least one point is required in ~s" sexpr)]
            [else(Poly (list->poly-list x)(list->poly-list y)) ]
        )]

      [else (error 'pars-sexpr "bad syntax in ~s" sexpr)]))


  (: parse : String -> PLANG)
(define (parse str) 
    (let ([code (string->sexpr str)]) 
      (pars-sexpr code)))

#|TESTS Q(3 - B2)|#
(test (parse "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}" ) => (Poly (list (Add (Num 0) (Num 1)) (Num 1) (Mul (Num 0) (Num 9))) (list (Sub (Num 4) (Num 5)) (Num 3) (Div (Num 27) (Num 9))))) 
(test (parse "{{poly {/ 4 2}  {- 4 1}} {{- 8 4}}}" ) =>(Poly (list (Div (Num 4) (Num 2)) (Sub (Num 4) (Num 1))) (list (Sub (Num 8) (Num 4)))))
(test (parse "{{poly 1 2 3} {1 2 3}}")  
     => (Poly (list (Num 1) (Num 2) (Num 3))  
              (list (Num 1) (Num 2) (Num 3)))) 
(test (parse "{{poly } {1 2} }")  
     =error> "parse: at least one coefficient is required in ((poly) (1 2))") 
(test (parse "{{poly 1 2} {} }")=error> "parse: at least one point is required in ((poly 1 2) ())")
(test (parse "{{poly 4/5 } {1/2 2/3 3} {poly 1 2 4} {1 2}}") =error> "bad syntax" )
 (test (parse "{{poly 1 2} {a b}}")=error> "bad syntax")
(test (parse "{{poly 1 2} {+ 5 7 8}}")=error> "bad syntax1")
(test (parse "{{poly 1 2} {: 5 8}}")=error> "bad syntax1")
(test (parse "{{poly 1 2} {+ a 8}}")=error> "bad syntax1")
(test (parse "{{poly 1 2} {+ 5 7 8}}")=error> "bad syntax1")
(test (parse "{{poly 1 2} {+ 5 7 8}}")=error> "bad syntax1")
(test (parse "{{poly a 2} {+ 5 7 8}}")=error> "bad syntax1")
(test (parse "{{poly + 4 2} {+ 5 8}}")=error> "bad syntax1")
(test (sexpr '((+ 4  7 2) (+ 5 8)))=error> "bad syntax2")

#|
_____________________________________________ END Q(3 B2)_____________________________________________________________________
|#


#|
_____________________________________________ Q(3 B3)_____________________________________________________________________
|#


#|
* @brief - This function evaluates the polynomial. She uses a map
  It took me an hour.
  that's a good question
|#




;; evaluates AE expressions to numbers
   (: eval : AE -> Number)
  (define (eval expr) 
    (cases expr 
      [(Num n)  n] 
      [(Add l r) (+ (eval l) (eval r))] 
      [(Sub l r) (- (eval l) (eval r))] 
      [(Mul l r) (* (eval l) (eval r))] 
      [(Div l r) (/ (eval l) (eval r))]))

  (: eval-poly : PLANG ->   (Listof Number) ) 
  (define (eval-poly p-expr)
    (cases p-expr
    [(Poly x y) (map (createPolynomial (map eval x)) (map eval y))]))
 
  (: run : String -> (Listof Number)) 
  ;; evaluate a FLANG program contained in a string 
  (define (run str) 
    (eval-poly (parse str))) 


#|TESTS Q(3 - c)|#
(test (run "{{poly 1 1 0} {-1 3 3}}")  => '(0 4 4))
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") => '(14))
(test (run "{{poly 4/5 } {1/2 2/3 3}}")  => '(4/5 4/5 4/5))
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}") => '(0 4 4)) 
(test (run "{{poly 1 2 3} {1 2 3}}")  => '(6 17 34))
(test (run "{{poly 4 2 7} {1 4 9}}")  => '(13 124 589)) 
(test (run "{{poly 1 2 3} {1 2 3}}")   => '(6 17 34)) 
(test (run "{{poly 1} {3}}")  => '(1))




#|
_____________________________________________ END Q( B3)_____________________________________________________________________
|#