#lang pl

;;  208236315_313131476


#|
  Expanding the FLANG BNF languager [2]
  
  # It took me half an hour
  # It's a nice exercise
  # We did this exercise together
  # It describes the grammar

|#

#| The grammar: 
 <FLANG> ::= <num> ;; Rule 1 
 | { + <FLANG> <FLANG> } ;; Rule 2 
 | { - <FLANG> <FLANG> } ;; Rule 3 
 | { * <FLANG> <FLANG> } ;; Rule 4 
 | { / <FLANG> <FLANG> } ;; Rule 5 
 | { with { <id> <FLANG> } <FLANG> } ;; Rule 6 
 | <id> ;; Rule 7 
 | { fun { <id> } <FLANG> } ;; Rule 8 
 | { call <FLANG> <FLANG> } ;; Rule 9 
 | True ;; Rule 10 
 | False ;; Rule 11 
 | { = <FLANG> <FLANG> } ;; Rule 12 
 | { not <FLANG> } ;; Rule 13 
 | { < <FLANG> <FLANG> } ;; Rule 14 
 | { > <FLANG> <FLANG> } ;; Rule 15 
 | { if <FLANG> { then-do <FLANG>} {else-do <FLANG>} } ;; Rule 16

|#


#|
 
 Extending the Parser [3]
 
 # It took an hour
 # This is an excellent exercise
 # We did this exercise together
 # This is for to convert s-expressions into FLANGs

|#

(define-type FLANG
  [Num  Number]
  [Add  FLANG FLANG]
  [Sub  FLANG FLANG]
  [Mul  FLANG FLANG]
  [Div  FLANG FLANG]
  [With  Symbol FLANG FLANG];; name, named-expr, body
  [Id  Symbol]
  [Fun  Symbol FLANG] ;;parameter-name, body
  [Call  FLANG FLANG]
  [Bool  Boolean ]
  [Bigger  FLANG FLANG]
  [Smaller  FLANG FLANG]
  [Equal  FLANG FLANG]
  [Not  FLANG]
  [If  FLANG FLANG FLANG]
  )


(: parse-sexpr : Sexpr -> FLANG)
;; to convert s-expressions into FLANGs
(define (parse-sexpr sxp)
  (match sxp
    [(number: n)    (Num n)]
    ['True (Bool true)]
    ['False (Bool false)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sxp
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad with syntax in ~s!!" sxp)])]
    [(cons 'fun more)
     (match sxp
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad fun syntax in ~s!!" sxp)])]
    [(list '+ l r) (Add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r) (Sub (parse-sexpr l) (parse-sexpr r))]
    [(list '/ l r) (Div (parse-sexpr l) (parse-sexpr r))]
    [(list '* l r) (Mul (parse-sexpr l) (parse-sexpr r))]
    [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
    [(list '= l r) (Equal (parse-sexpr l) (parse-sexpr r))]
    [(list '> l r) (Bigger (parse-sexpr l) (parse-sexpr r))]
    [(list '< l r) (Smaller (parse-sexpr l) (parse-sexpr r))]
    [(list 'not exp) (Not (parse-sexpr exp))]
    [(cons 'if rest)
     (match sxp 
       [(list 'if l (list 'then-do m) (list 'else-do r)) (If (parse-sexpr l) (parse-sexpr m) (parse-sexpr r))]
       [else (error 'parse-sexpr "bad if syntax in ~s" sxp)])]
    [else (error 'parse-sexpr "bad syntax in ~s" sxp)]))


;;String -> FLANG
(: parse : String -> FLANG)
(define (parse str)
  (parse-sexpr (string->sexpr str)))


;;tests
(test (parse "4") => (Num 4))
(test (parse "{+ 3 5}") => (Add (Num 3) (Num 5)))
(test (parse "{+ 3 {- 8 {+ 2 1}}}") => (Add (Num 3) (Sub (Num 8) (Add (Num 2) (Num 1)))))
(test (parse "{+ 1 2 3}") =error> "bad syntax")
(test (parse "{with {x {+ 4 2}} {* x x}}") => (With 'x (Add (Num 4) (Num 2))(Mul (Id 'x) (Id 'x))))
(test (parse "{fun {x} x}") => (Fun 'x (Id 'x)))
(test (parse "{fun {x} {/ x 5}}") => (Fun 'x (Div (Id 'x) (Num 5))))
(test (parse "{call {fun {x} {/ x 5}} 8}") => (Call {Fun 'x (Div (Id 'x) (Num 5))} (Num 8)))
(test (parse "{with {sqr {fun {x} {* x x}}}{+ {call sqr 5}{call sqr 6}}}") =>(With 'sqr (Fun 'x (Mul (Id 'x) (Id 'x)))(Add (Call (Id 'sqr) (Num 5))(Call (Id 'sqr) (Num 6)))))
(test (parse "{fun x {* x x}}")=error> "bad fun syntax")




#|
 Extending subst and eval  [4]


# It took six hours
# It's a tough exercise
# This is an exercise of Substitution rules and Evaluation rules


Formal Substitution rules:
subst: 
   N[v/x] = N 
   {+ E1 E2}[v/x] = {+ E1[v/x] E2[v/x]} 
   {- E1 E2}[v/x] = {- E1[v/x] E2[v/x]} 
   {* E1 E2}[v/x] = {* E1[v/x] E2[v/x]} 
   {/ E1 E2}[v/x] = {/ E1[v/x] E2[v/x]} 
   y[v/x] = y 
   x[v/x] = v 
  {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x 
  {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2} 
  {call E1 E2}[v/x] = {call E1[v/x] E2[v/x]} 
  {fun {y} E}[v/x] = {fun {y} E[v/x]} ; if y =/= x 
  {fun {x} E}[v/x] = {fun {x} E} 
  B[v/x] = B ;; B is Boolean 
  {= E1 E2}[v/x] = {= E1[v/x] E2[v/x]} 
  {> E1 E2}[v/x] = {> E1[v/x] E2[v/x]} 
  {< E1 E2}[v/x] = {< E1[v/x] E2[v/x]} 
  { not E}[v/x] = {not E[v/x]} 
  {if Econd {then-do Edo} {else-do Eelse}}[v/x] = {if Econd[v/x] {then-do Edo[v/x]} {else-do Eelse[v/x]}} 

eval: Evaluation rules:
    eval(N) = N ;; N is an expression for a numeric value 
    eval({+ E1 E2}) = eval(E1) + eval(E2) \ if both E1 and E2 
    eval({- E1 E2}) = eval(E1) - eval(E2) \ evaluate to numbers 
    eval({* E1 E2}) = eval(E1) * eval(E2) / otherwise error! 
    eval({/ E1 E2}) = eval(E1) / eval(E2) / 
    eval(id) = error!
    eval({with {x E1} E2}) = eval(E2[eval(E1)/x]) 
    eval(FUN) = FUN ; assuming FUN is a function expression 
    eval({call E1 E2}) = eval(Ef[eval(E2)/x]) 
    if eval(E1)={fun {x} Ef} = error! otherwise


|#


(: subst : FLANG Symbol FLANG -> FLANG)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(With name named body)(With name (subst named from to)
        (if (eq? from name)body(subst body from to)))]
    [(Fun name body)(Fun name (if (eq? name from)body(subst  body from to)))]
    [(Call fun-expr arg-expr)  (Call (subst fun-expr from to) (subst arg-expr from to))]
    [(Id name) (if (eq? from name) to expr)]
    [(Bool b) expr]
    [(Equal l r) (Equal (subst l from to) (subst r from to))]
    [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
    [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
    [(Not a) (Not(subst a from to))]
    [(If l m r) (If(subst l from to)(subst m from to)(subst r from to))]
    ))



;;tests
(test (subst (Mul (Id 'x) (Id 'x))'x (Num 6)) => (Mul (Num 6) (Num 6)))
(test (subst (Id 'x)'x(Num 8)) => (Num 8))
(test (subst (Id 'y)'x(Num 8)) => (Id 'y))
(test (subst (With 'x (Num 3)(Id 'x))'x(Num 5)) => (With 'x (Num 3)(Id 'x)))
(test (subst (With 'y(Add (Id 'x) (Num 3))(Add (Id 'x) (Num 5)))'x(Num 4)) => (With 'y(Add (Num 4) (Num 3))(Add (Num 4) (Num 5))))
(test (subst (Fun 'x (Add (Id 'x) (Id 'y)))'x(Num 4)) => (Fun 'x (Add (Id 'x) (Id 'y))))
(test (subst (Fun 'x (Add (Id 'x) (Id 'y)))'y(Num 4)) => (Fun 'x (Add (Id 'x) (Num 4))))
(test (subst (Call (Fun 'x (Div (Id 'x) (Id 'y)))(Add (Id 'x) (Id 'y)))'x(Num 3)) => (Call (Fun 'x (Div (Id 'x) (Id 'y)))(Add (Num 3) (Id 'y))))
(test (subst (Call (Fun 'x (Div (Id 'x) (Id 'y)))(Add (Id 'x) (Id 'y)))'y(Num 3)) => (Call (Fun 'x (Div (Id 'x) (Num 3)))(Add (Id 'x) (Num 3))))




(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; gets a Racket numeric binary operator, and uses it within a FLANG
;; `Num' wrapper
(define (arith-op op expr1 expr2)
  (Num (op (Num->number expr1) (Num->number expr2))))

(: Num->number : FLANG -> Number)
(define (Num->number e)
  (cases e
    [(Num n) n]
    [else (error 'Num->number "expected a number, got: ~s" e)]))

(: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
;; gets a Racket Boolean binary operator (on numbers), and applies it
;; to two Num' wrapped FLANGs
(define (logic-op op expr1 expr2)
  (Bool (op ( Num->number expr1) ( Num->number expr2))))

(: flang->bool : FLANG -> Boolean)
;; gets a Flang E (of any kind) and returns a its appropiate
;; Boolean value -- which is true if and only if E does not
;; represent false
;; Remark: the flang->bool function will also be top-level
;; since it's used in more than one place.
(define (flang->bool e)
  (cases e
    [(Bool b) b]
    [else #t]))


(: eval : FLANG -> FLANG)
;; evaluates FLANG expressions by reducing them to *expressions*
(define (eval expr)
  (cases expr
    [(Num n) expr]
    [(Add l r) (arith-op + (eval l) (eval r))]
    [(Sub l r) (arith-op - (eval l) (eval r))]
    [(Mul l r) (arith-op * (eval l) (eval r))]
    [(Div l r) (arith-op / (eval l) (eval r))]
    [(With name named body) (eval (subst body name (eval named)))]
    [(Id name) (error 'eval "free identifier: ~s" name)]
    [(Fun name body) expr]
    [(Call fun-expr arg-expr) (let ([fval (eval fun-expr)])
                               (cases fval
                                 [(Fun name body) (eval (subst body name(eval arg-expr)))]
                                 [else (error 'eval "expected a function, got: ~s" fval)]))]
    [(Bool b) expr]
    [(Equal l r) (logic-op = (eval l) (eval r))]
    [(Bigger l r) (logic-op > (eval l) (eval r))]
    [(Smaller l r) (logic-op < (eval l) (eval r))]
    [(If l m r)
     (let ([res (eval l)])
       (if (flang->bool res) (eval m) (eval r)))]
    [(Not exp) (Bool(not (flang->bool(eval exp))))])) 



;;tests
(test (eval (Call (Fun 'x (Mul (Id 'x) (Num 4)))(Num 3))) => (Num 12))
(test (eval (Call (With 'foo(Fun 'x (Mul (Id 'x) (Num 4)))(Id 'foo))(Num 3))) => (Num 12))


#|
  Extending the run procedure  [5]
  
  #This is the main function to activate
  #It took 20 minutes
  #This is an easy exercise
  # We did it together
  # we will allow the interface procedure to return any one of the three possible types of the extended language
|#




(: run : String -> (U Boolean Number FLANG))
;; evaluate a FLANG program contained in a string
(define (run str)
  (let ([result (eval (parse str))])
    (cases result
      [(Num nu) nu]
      [(Bool bo) bo]
      [else result])))




;; tests 
(test (run "True") => true)
(test (run "False") => false)
(test (run "{not True}") => false)
(test (run "{not False}") => true)
(test (run "{> 3 44}") => false)
(test (run "{= 5 5}") => true)
(test (run "{< 4 5}") => true)
(test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4) 
(test (run "{with {x 8}{if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
(test (run "{with {x 0}{if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0) 
(test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true) 
(test (run "{with {c True}{if c {then-do {> 2 1}} {else-do 2}}}") => true) 
(test (run "{with {foo {fun {x} {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}") => (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2))))) 
(test (run "{with {x 0} {if {> x 0} {/ 2 x} x}}") =error> "parse-sexpr: bad if syntax in (if (> x 0) (/ 2 x) x)") 
(test (run "true") =error> "eval: free identifier: true") 
(test (run "{< false 5}") =error> "eval: free identifier: false") 
(test (run "{< False 5}") =error> "Num->number: expected a number, got: #(struct:Bool #f)")
(test (run "{with {x 1} {if {< x 2} {then-do True} {else-do x}}}") => #t)
(test (run "{with {x 1} {if {not {= x 2}} {then-do True} {else-do x}}}") => #t)
(test (run "5") => 5)
(test (run "{+ 4 6}") => 10)
(test (run "{+ 4 6}") => 10)
(test (run "{call {fun {x} {+ x 1}} 10}") => 11)
(test (run "{call {fun {x} {+ x 7}} {with {x 8} {+ x 7}}}") => 22)
(test (run "{with {x {- 1 6}} {if {< x 0} {then-do {- 6 x}} {else-do x}}}") => 11)
(test (run "{if {= x x} {- x x} x}") =error> "parse-sexpr: bad if syntax in (if (= x x) (- x x) x)")
(test (run "{with y {with {{x x} x} y}}") =error> "bad with syntax in")
(test (run "{call {with {x 1} {with {x x} x}} 1}") =error> "eval: expected a function, got: #(struct:Num 1)")
(test (run "{with {x 1} {if {x} {then-do {/ x x}} {else-do {* x x}}}}") =error> "parse-sexpr: bad syntax in (x)")
(test (run "{call {fun {0} {+ x x}} 1}") =error> "parse-sexpr: bad fun syntax in")
(test (run "{= True 5}") =error> "Num->number: expected a number, got: #(struct:Bool #t)")
(test (run "true") =error> "eval: free identifier: true")
















