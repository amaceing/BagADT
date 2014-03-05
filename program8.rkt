#lang racket
;Anthony Mace CSC240 2/26/14
;Program 8

(define (getValue pair)
  (if (null? pair)
      '()
      (car pair)
   )
)

(define (getCount pair)
  (if (null? pair)
      '()
      (cdr pair)
   )
)

(define (newPair item)
  (cons item 1)
)

(define (incPair pair)
  (if (null? pair)
      '()
      (cons (car pair) (+ (cdr pair) 1))
   )
)

(define (decPair pair)
  (if (null? pair)
      '()
      (if (= (cdr pair) 1)
          '()
          (cons (car pair) (- (cdr pair) 1))
       )
   )
)

(define (insertBag myList item)
  (if (null? myList)
      (cons (newPair item) myList)
      (if (string=? (getValue (car myList)) item)
          (cons (incPair (car myList)) (cdr myList))
          (cons (car myList) (insertBag (cdr myList) item))
      )
   )
)

(define (getBagCount myList item)
  (if (null? myList)
      0
      (if (string=? (getValue (car myList)) item)
          (getCount (car myList))
          (getBagCount (cdr myList) item)
      )
   )
)

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

(define (deleteAllBag myList item)
  (if (null? myList)
      '()
      (if (string=? (getValue (car myList)) item)
          (cdr myList)
          (cons (car myList) (deleteAllBag (cdr myList) item))
      )
  )
)

(define (unionBag myListA myListB)
  (if (null? myListA)
      myListB
      (if (null? myListB)
          myListA
          (if (string=? (getValue (car myListA)) (getValue (car myListB)))
              (cons
               (addPairs
                (car myListA)
                (car myListB)
                )
               (unionBag (cdr myListA) (cdr myListB))
              )
              (cons (car myListA) (cons (car myListB) (unionBag (cdr myListA) (cdr myListB))))
          )
      )
   )
)

(define (addPairs pairA pairB)
  (cons 
   (getValue pairA)
   (+
    (getCount pairA)
    (getCount pairB)
   )
  )
)

(unionBag '(("a" . 2) ("c" . 3) ("d" . 3)) '(("a" . 3) ("d" . 4) ("c" . 3)))

(addPairs '("a" . 2) '("a" . 3))
