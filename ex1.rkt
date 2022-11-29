#lang pl

#|
*@brief This is the first assignment in a programming language
*AUTHORS: Niv Kotek - 208236315
*@date 2022-11-20
|#




;________________________Q(1 - a)_______________________________________

#|
* @brief - This is a function that takes a list of lists and turns them into a single list
 This is a question that took me 10 minutes
  This is a boring question
* @param list - A list of lists
* @return Opens the list to a single list
|#

( : open-list : (Listof (Listof Number) ) -> (Listof Number) )
(define (open-list list)
   (if (null? list) '()
       (append (first list)(open-list(rest list) ) )
    )
)

#|TESTS Q(1 - a)|#
(test (open-list '((1 2 3)(4 5 6 )(7 8 9))) =>'(1 2 3 4 5 6 7 8 9))
(test (open-list '((1)(2 3 4 5 6 7 8 9)())) =>'(1 2 3 4 5 6 7 8 9))
(test (open-list '(()()())) =>'())
(test (open-list '((0)(0 0 0 0 0 0)()(0 0 0))) =>'(0 0 0 0 0 0 0 0 0 0))
(test (open-list '(()(1))) =>'(1))

;________________________END(1 - a)_______________________________________





;________________________Q(1 - b)_______________________________________

#|
* @brief  - This is a helper function for finding minimum and maximum
* @param lst -  A list of lists
* @param minMum - the smallest number
* @param maxMum - the largest number
* @return - A vector of minimum and maximum
|#


( : Min&Max :  (Listof Number) Number Number  -> (Listof Number))
(define (Min&Max lst minMum maxMum)
  (if (null? lst) (list minMum maxMum)
      (Min&Max(rest lst)(min minMum(first lst))(max maxMum(first lst)))
  )
)


#|
* @brief - This function finds the minimum and maximum from a list of lists. It uses a helper function
 It took me half an hour to write
 That's an okay question
* @param lst - A list of lists
* @return A vector of minimum and maximum
|#

( : min&max : (Listof (Listof Number) ) -> (Listof Number) )
(define (min&max lst)
    (if (null? (open-list lst) ) (list -inf.0 +inf.0)
       (Min&Max(open-list lst) +inf.0 -inf.0)
    )
 )

#|TESTS Q(1 - b)|#
(test (min&max '((1 2 3)(4 5 6 )(7 8 9))) =>'(1.0 9.0))
(test (min&max '((5 7 4 9 -5)(5 3 22.3 )(4 5 7.7))) =>'(-5.0 22.3))
(test (min&max '(()()())) =>'(-inf.0 +inf.0))
(test (min&max '((7.8)()())) =>'(7.8 7.8))
(test (min&max '((-4.4 -77 4)(0 0 0)(8 5 4 3 8))) =>'(-77.0 8.0))
(test (min&max '((5 7 8)(5 4 2)(5))) => '(2.0 8.0))
(test (min&max '()) => '(-inf.0 +inf.0))
;________________________END(1 - b)_______________________________________





;________________________Q(1 - c)_______________________________________


#|
* @brief - It is a function that finds minimum and maximum by the apply function
  This is an interesting question because I didn't know how to use apply
  It took me 10 minutes
* @param lst - A list of lists
* @return A vector of minimum and maximum
|#


( : min&max_apply : (Listof (Listof Number) ) -> (Listof Number) )
(define (min&max_apply lst)
    (if (null? (open-list lst) ) (list -inf.0 +inf.0)
        (list (apply min (open-list lst))(apply max(open-list lst) ) )
    )
)

#|TESTS Q(1 - c)|#
(test (min&max_apply '((1 -2 3)(4 5 6)(255 7 8 0)))=>'(-2 255))
(test (min&max_apply '(()()()))=>'(-inf.0 +inf.0))
(test (min&max_apply '((0)()()))=>'(0 0))
(test (min&max_apply '(()))=>'(-inf.0 +inf.0))
(test (min&max_apply '((1 -2.2 3)(4 5 6)(255 7 8 0)))=>'(-2.2 255.0))
(test (min&max_apply '((1)(-1)()))=>'(-1 1))


;________________________END(1 - c)_______________________________________




;~~~~~~~~~~~~~~~~~~~~~~2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#|
For part number 2 I write 2 ways to solve it
The first solution took me 6 hours for the second solution after 3 days of thought it took me an hour to write the code.
I liked the second solution more.
|#


;____________________________________________________First attempt______________________________________________

#|
;________________________Q(2 - a)_______________________________________

#|
* @brief - This describes the table. An empty table or a table with lists. It took me half an hour. And that's a good question
* @param EmptyTbl - An empty table with a deductive constructor
* @param Tbl - A table with a list of Symbol String lists
* @return 
|#


(define-type Table
  [EmptyTbl]
  [Tbl (Listof (Listof ( U Symbol String)))]
  )

#|TESTS Q(2 - a)|#
(test (EmptyTbl)=>(EmptyTbl))
(test (Tbl (list (list 'a "a") (list 'b "b")))=>(Tbl (list (list 'a "a") (list 'b "b"))))

;________________________END(2 - a)_______________________________________



;________________________Q(2 - b)_______________________________________


#|
* @brief - This is a function to add.
  It took me half an hour.
  This is a question I liked less
* @param sym - index (key)
* @param str - the string (value)
* @param tab - The table
* @return table after the addition
|#


( : Add : Symbol String Table -> Table)
(define (Add sym str tab)
  (cases tab
    [(Tbl ll) (Tbl(append(list(list sym str)) ll))]
    [else (Tbl (list(list sym str)))]
   )
 )

#|TESTS Q(2 - b)|#
(test (Add 'b "B"(Add 'a "A"(EmptyTbl)))=>(Add 'b "B"(Add 'a "A"(EmptyTbl))))
(test (Add 'b "B" (Add 'a "A" (EmptyTbl))) =>(Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) =>  (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))

;________________________END(2 - b)_______________________________________


;________________________Q(2 - c)_______________________________________

#|
* @brief - Help function for searching
* @param lst - A list of lists (of Symbol String)
* @param item - The key to finding value
* @return The value found
|#


(: help-search-table : (Listof (Listof ( U Symbol String) ) ) Symbol -> ( U String Boolean Symbol) )
(define (help-search-table lst item)
  (cond [(null? lst) #f]
        [(equal? (first (first lst)) item) (first(rest (first lst)))]
        [else (help-search-table (rest lst) item)]
   )
 )

#|
* @brief - This is a function to search by key.
  The function is assisted by a helper function.
  It took me an hour. And that's a good question
* @param sym - The key to finding value
* @param tab - The table
* @return The value found
|#

(: search-table : Symbol Table -> ( U String Boolean Symbol ))
(define (search-table sym tab)
        (cases tab
          [(Tbl ll)  (help-search-table ll sym)]
          [else #f]
         )
  )

#|TESTS Q(2 - c)|#
(test (search-table 'a (Add 'a "AAA"(Add 'b "B" (Add 'a "A" (EmptyTbl)))))=>"AAA")
(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))=> #f)
(test (search-table 'a (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))=> "AAA")
;________________________END(2 - c)_______________________________________



;________________________Q(2 - d)_______________________________________


#|
* @brief - helper function for deletion
* @param lst - A list of lists (of Symbol String)
* @param item - The key to delete the entry
* @return List after deletion
|#

(: help-remove-item : (Listof (Listof ( U Symbol String) ) ) Symbol -> (Listof (Listof ( U Symbol String) )))
(define (help-remove-item lst item)
  (cond [(null? lst) null]
        [(equal? (first (first lst)) item) (rest lst)]
        [else (append (list (first lst)) (help-remove-item (rest lst) item))]
   )
 )


#|
* @brief It is a function that deletes the table according to the key.
  In this function I used a helper function.
  It's a function that took me an hour. That's a nice question
* @param tab - The table from which to delete
* @param item - The key to delete the entry
* @return The table after the deletion
|#

( : remove-item : Table Symbol -> Table)
(define (remove-item tab item)
        (cases tab
          [(Tbl ll) (Tbl (help-remove-item ll item))]
          [else tab]
        )
 )


#|TESTS Q(2 - d)|#
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'a) =>(Add 'b "B" (Add 'a "A"(EmptyTbl))))
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" 
(EmptyTbl)))) 'a)=> (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'b)=> (Add 'a "AAA" (Add 'a "A" (EmptyTbl))))

;________________________END(2 - d)_______________________________________


|#






;____________________________________________________Second attempt______________________________________________

; The second attempt - a better solution




;________________________Q(2 - a) & Q(2 - b)_______________________________________

#|
* @brief - It describes some class of table of data on key and value
  take me 30 min.
  very nice quation
* @param EmptyTbl - An empty table with a deductive constructor 
* @param Add - Constructor of a table with a value key and another table (like a list)
|#

(define-type Table
  [EmptyTbl]
  [Add Symbol String Table])

#|TESTS Q(2 - a) & Q(2 - b)|#
(test (EmptyTbl) => (EmptyTbl))
(test (Add 'b "B" (Add 'a "A" (EmptyTbl))) => (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) => (Add 'a "aa" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))
(test (Add 'a "A" (Add 'a "A" (EmptyTbl))) => (Add 'a "A" (Add 'a "A" (EmptyTbl))))
(test (Add 'b "" (Add 'a "" (EmptyTbl))) => (Add 'b "" (Add 'a "" (EmptyTbl))))
;________________________END Q(2 - a) & Q(2 - b)_______________________________________


;________________________Q(2 - c)_______________________________________


#|
* @brief - This is a function to search by key.
  It took me an hour.
  that's a good question
* @param symbol - The key to finding value
* @param tbl - The table
* @return The value found or #f(not found value)
|#

(: search-table : Symbol Table -> ( U String #f ))
(define (search-table symbol tbl)
        (cases tbl
          [(EmptyTbl) #f]
          [(Add key val tblI) (if (equal? key symbol) val
                                  (search-table symbol tblI))]
        )
 )

#|TESTS Q(2 - c)|#
(test (search-table 'c (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))=> #f)
(test (search-table 'a (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))=> "AAA")
(test (search-table 'a (Add 'a "" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))=> "")
(test (search-table 'a (Add 'a "A" (Add 'a "B" (Add 'a "C" (EmptyTbl)))))=> "A")
(test (search-table 'c (EmptyTbl))=> #f)


;________________________END Q(2 - c)_______________________________________

;________________________Q(2 - d)_______________________________________

#|
* @brief It is a function that deletes the table according to the key.
  It's a function that took me an hour.
  That's a nice question
* @param tbl - The table from which to delete
* @param symbol - The key to delete the entry
* @return The table after the deletion
|#


( : remove-item : Table Symbol -> Table)
(define (remove-item tbl symbol)
        (cases tbl
          [(EmptyTbl) tbl]
          [(Add key val tblI) (if (equal? key symbol) tblI
                                  (Add key val (remove-item tblI symbol)))]
        )
 )

#|TESTS Q(2 - d)|#
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'a)=> (Add 'b "B" (Add 'a "A" (EmptyTbl))))
(test (remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'b)=> (Add 'a "AAA" (Add 'a "A" (EmptyTbl))))
(test (remove-item (EmptyTbl) 'a)=> (EmptyTbl))
(test(remove-item (Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))) 'c)=>(Add 'a "AAA" (Add 'b "B" (Add 'a "A" (EmptyTbl)))))
(test(remove-item(Add 'a "AAA" (Add 'b "B" (Add 'c "A" (EmptyTbl)))) 'c )=>(Add 'a "AAA" (Add 'b "B"(EmptyTbl))))

;________________________END(2 - d)_______________________________________


