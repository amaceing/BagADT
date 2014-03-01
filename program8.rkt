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
  (if (= (getBagCount myList item) 0)
      (cons (newPair item) myList)
      (if (> (getBagCount myList item) 0)
          (cons (car myList) myList)
          (insertBag (cdr myList) item)
       )
   )
)
       

(define (findPair myList item)
  (if (string=? (getValue (car myList)) item)
      (car myList)
      (findPair (cdr myList) item)
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


(insertBag '(("a" . 2) ("c" . 4) ("b" . 4)) "c")

(deletePair '(("a" . 2) ("c" . 4) ("b" . 4)) '("c" . 4))
