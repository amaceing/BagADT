#lang racket
;Anthony Mace CSC240 2/26/14
;This program contains a library of 
;functions that operate on/manipulate a Bag of strings

;Returns the charcter value of the pair
(define (getValue pair)
  (if (null? pair)
      '()
      (car pair)
   )
)

;Returns the number of occurences for the pair
(define (getCount pair)
  (if (null? pair)
      '()
      (cdr pair)
   )
)

;Creates a new pair
(define (newPair item)
  (cons item 1)
)

;Increments the number of occurences by 1
(define (incPair pair)
  (if (null? pair)
      '()
      (cons (car pair) (+ (cdr pair) 1))
   )
)

;Decrements the number of occurences by 1
(define (decPair pair)
  (if (null? pair)
      '()
      (if (= (cdr pair) 1)
          '()
          (cons (car pair) (- (cdr pair) 1))
       )
   )
)

;Inserts an item into the bag
(define (insertBag myList item)
  (if (null? myList)
      (cons (newPair item) myList)
      (if (string=? (getValue (car myList)) item)
          (cons (incPair (car myList)) (cdr myList))
          (cons (car myList) (insertBag (cdr myList) item))
      )
   )
)

;Returns the number of occurences of the
;item in the bag
(define (getBagCount myList item)
  (if (null? myList)
      0
      (if (string=? (getValue (car myList)) item)
          (getCount (car myList))
          (getBagCount (cdr myList) item)
      )
   )
)

;Returns a bag that represents decrementing
;the item in the bag
(define (deleteBag myList item)
  (if (null? myList)
      '()
      (if (string=? (getValue (car myList)) item)
          (if (= (getCount (car myList)) 1)
              (cdr myList)
              (cons (decPair (car myList)) (cdr myList))
          )
          (cons (car myList) (deleteBag (cdr myList) item))
      )
   )
)

;Returns a bag that represents completely
;deleting all occurences of the item from the bag
(define (deleteAllBag myList item)
  (if (null? myList)
      '()
      (if (string=? (getValue (car myList)) item)
          (cdr myList)
          (cons (car myList) (deleteAllBag (cdr myList) item))
      )
  )
)

;Combines the contents of both bags
(define (unionBag myListA myListB)
  (if (null? myListA)
      myListB
      (if (null? myListB)
          myListA
          (cons 
           (addMatches (car myListA) myListB) 
           (unionBag 
            (cdr myListA) 
            (deleteAllBag
              myListB
             (getValue (returnMatch (car myListA) myListB))
             )
            )
          )
       )
   )     
)

;Returns the intersection of both bags
(define (intersectBag myListA myListB)
  (if (or (null? myListA) (null? myListB))
      '()
      (if (null? (returnMatch (car myListA) myListB))
          (intersectBag (cdr myListA) myListB)
          (cons 
           (getMinFrequency (car myListA) (returnMatch (car myListA) myListB))
           (intersectBag (cdr myListA) myListB)
          )
       )
    )
)

;Adds pairs in the list that match to the given pair        
(define (addMatches pair myList)
  (if (or (null? pair) (null? myList))
      pair
      (if (string=? (getValue pair) (getValue (car myList)))
          (addPairs pair (car myList))
          (addMatches pair (cdr myList))
       )
   )
)
         
;Returns a match of the given pair in the list
(define (returnMatch pair myList)
  (if (null? myList)
      '()
      (if (string=? (getValue pair) (getValue (car myList)))
          (car myList)
          (returnMatch pair (cdr myList))
       )
   )
)

;Returns that pair with the smaller number of occurences
(define (getMinFrequency pairA pairB)
  (if (<= (getCount pairA) (getCount pairB))
      pairA
      pairB
  )
)

;Adds two pairs together
(define (addPairs pairA pairB)
  (cons 
   (getValue pairA)
   (+
    (getCount pairA)
    (getCount pairB)
   )
  )
)

(unionBag '(("a" . 3) ("r" . 3)) '(("e" . 3) ("r" . 4) ("a" . 2)))

(intersectBag '(("a" . 3) ("e" . 4)) '(("e" . 7) ("r" . 4) ("a" . 2)))

(addMatches '("a" . 3) '(("e" . 3) ("r" . 3) ("a" . 3)))

(addPairs '("a" . 4) '("a" . 2))

(getMinFrequency '("a" . 4) '("a" . 2))
