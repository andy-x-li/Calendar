#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

(require typed/test-engine/racket-tests)

;; Name: Andy Li 

(define-struct CalFormat
  ([cell-size : Integer]
   [title-bar-bg : Image-Color]
   [title-bar-font : Image-Color]
   [title-bar-height : Integer]
   [day-label-bg : Image-Color]
   [day-label-font : Image-Color]
   [day-label-height : Integer]
   [cell-bg : Image-Color]
   [cell-font : Image-Color]))

(define-struct CalWorld
  ([format : CalFormat]
   [current-month : Integer]
   [current-year : Integer]))

;; TAKEN FROM LAB2 - Lab Partner: Haichuan Wang
(define-struct Date
  ([m : Integer]
   [d : Integer]
   [y : Integer]))

;; TAKEN FROM LAB2 - Lab Partner: Haichuan Wang
(define-struct Day-Of-Week
  ([num : Integer]))

(: fmt0 CalFormat)
;; fmt0 is an example format for testing purposes
;;
(define fmt0
  (CalFormat 40
             'dodgerblue 'lightyellow 60
             'silver 'blue 30
             'lightyellow 'black))

(: month-text : Integer -> String)
;; This is a helper function for background-layout.
;; month-text takes in an integer from 1-12 inclusive and returns the month in 
;; the form of a string.
;;
(define (month-text n)
  (match n
    [1 "January"]
    [2 "February"]
    [3 "March"]
    [4 "April"]
    [5 "May"]
    [6 "June"]
    [7 "July"]
    [8 "August"]
    [9 "September"]
    [10 "October"]
    [11 "November"]
    [12 "December"]
    [_ "None"]))

(check-expect (month-text 4) "April")
(check-expect (month-text 12) "December")

(: background-layout : CalFormat Integer Integer -> Image)
;; background-layout is a helper function for draw-month that takes in a  
;; CalFormat and two integers (month and year) and draws the title bar along 
;; with the day labels.
;;
(define (background-layout fmt m y)
  (match fmt
    ['() empty-image]
    [(CalFormat cell-size bar-c bar-fc bar-h day-c day-fc day-h cell-c cell-fc)
     (above
      (overlay
       (beside 
        (text (string-append
               (month-text m)
               " ") (cast (* 1/2 bar-h) Byte) bar-fc)
        (text (number->string y) (cast (* 1/2 bar-h) Byte) bar-fc))
       (rectangle (* 7 cell-size) bar-h  'outline 'black)
       (rectangle (* 7 cell-size) bar-h  'solid bar-c))
      (labels fmt (list "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")))]))

(: labels : CalFormat (Listof String) -> Image)
;; labels is a helper function for day-layout that draws the day labels using 
;; CalFormat variables and a list of strings that contains the days needed to
;; be drawn.
;;
(define (labels fmt str-day)
  (match fmt
    ['() empty-image]
    [(CalFormat cell-size _ _ _ day-c day-fc day-h _ _)
     (match str-day
       ['() empty-image]
       [(cons head tail) (beside
                          (overlay
                           (text head (cast (* 1/2 day-h) Byte) day-fc)
                           (rectangle cell-size day-h 'outline 'black)
                           (rectangle cell-size day-h 'solid day-c))
                          (labels fmt tail))])]))

(: leap? (-> Integer Boolean))
;; TAKEN FROM LAB2 - Lab Partner: Haichuan Wang
;; This is a helper function for day-layout. 
;; This operation takes in an integer that represents the year (Year) and 
;; calculates if it's a leap year or not. If the year divides evenly by 4 and 
;; it's not divisible by 100, or if it's divisible by 400, then it is a leap 
;; year and the operation returns #t. Otherwise, it returns #f.
;;
(define (leap? Year)
  (or (and (= (remainder Year 4) 0)
           (not (= (remainder Year 100) 0)))
      (= (remainder Year 400) 0)))

(check-expect (leap? 2020) #t)
(check-expect (leap? 2000) #t)
(check-expect (leap? 1800) #f)
(check-expect (leap? 2021) #f)

(: days-in-month (-> Integer Integer Integer))
;; TAKEN FROM LAB2 - Lab Partner: Haichuan Wang
;; This is a helper function for day-layout. 
;; This operation takes in two integers that represent the month (Month) and
;; year (Year), in that order, and returns an integer that represents the
;; number of days in that month.
;;
(define (days-in-month Month Year)
  (match Month
    [1 31]
    [2 (cond 
         [(leap? Year) 29]
         [else 28])]
    [3 31]
    [4 30]
    [5 31]
    [6 30]
    [7 31]
    [8 31]
    [9 30]
    [10 31]
    [11 30]
    [12 31]))
       
(check-expect (days-in-month 2 2021) 28)
(check-expect (days-in-month 2 2020) 29)
(check-expect (days-in-month 12 2021) 31)

(: days-after (-> Day-Of-Week Integer Day-Of-Week))
;; TAKEN FROM LAB2 - Lab Partner: Haichuan Wang
;; This is a helper function for find-day-of-week. 
;; This operation takes in a day of the week (CurrentDay) and an integer (d), 
;; and finds a new day of the week by adding d to the original day of the week. 
;; 0-6 represent Sunday-Saturday, respectively. If the sum of the values 
;; exceeds 6, use recursion to calculate the accurate day of the week. The 
;; operation returns a Day-of-Week structure. 
;;
(define (days-after CurrentDay d)
  (local
    {(define Sum (+ (Day-Of-Week-num CurrentDay) d))}
    (cond
      [(> Sum 6) (days-after (Day-Of-Week (- Sum 7)) 0)]
      [(= Sum 0) (Day-Of-Week 0)]
      [(= Sum 1) (Day-Of-Week 1)]
      [(= Sum 2) (Day-Of-Week 2)]
      [(= Sum 3) (Day-Of-Week 3)]
      [(= Sum 4) (Day-Of-Week 4)]
      [(= Sum 5) (Day-Of-Week 5)]
      [else (Day-Of-Week 6)])))

(check-expect (days-after (Day-Of-Week 6) 13) (Day-Of-Week 5))
(check-expect (days-after (Day-Of-Week 1) 2) (Day-Of-Week 3))
(check-expect (days-after (Day-Of-Week 0) 14) (Day-Of-Week 0))
(check-expect (days-after (Day-Of-Week 6) 29) (Day-Of-Week 0))
(check-expect (days-after (Day-Of-Week 0) 15) (Day-Of-Week 1))

(: doomsday-in-month (-> Integer Integer Integer))
;; TAKEN FROM LAB2 - Lab Partner: Haichuan Wang
;; This is a helper function for find-day-of-week. 
;; Takes in a month (Month) and a year (Year), and this operator calculates the
;; day in the month that is a Doomsday and returns it.
;;
(define (doomsday-in-month Month Year)
  (match Month
    [1 (cond
         [(leap? Year) 32]
         [else 31])]
    [2 (cond
         [(leap? Year) 29]
         [else 28])]
    [3 0]
    [4 4]
    [5 9]
    [6 6]
    [7 11]
    [8 8]
    [9 5]
    [10 10]
    [11 7]
    [_ 12]))
         
(check-expect (doomsday-in-month 1 2021) 31)
(check-expect (doomsday-in-month 1 2020) 32)
(check-expect (doomsday-in-month 2 2021) 28)
(check-expect (doomsday-in-month 2 2020) 29)
(check-expect (doomsday-in-month 7 2021) 11)  

(: Century-Doomsday (-> Integer Day-Of-Week))
;; TAKEN FROM LAB2 - Lab Partner: Haichuan Wang
;; This is a helper function for find-day-of-week. 
;; This operation is an intermediate/helper operation used to determine the 
;; Doomsday of the century for a given year (Year). Takes in an integer and 
;; returns the Doomsday of the century as a Day-Of-Week struct. Local variable 
;; "Index" is used to match accordingly. 
;;
(define (Century-Doomsday Year)
  (local
    {(define Index (remainder (quotient Year 100) 4))}
    (match Index
      [0 (Day-Of-Week 2)]
      [1 (Day-Of-Week 0)]
      [2 (Day-Of-Week 5)]
      [_ (Day-Of-Week 3)])))

(check-expect (Century-Doomsday 1950) (Day-Of-Week 3))
(check-expect (Century-Doomsday 1800) (Day-Of-Week 5))
(check-expect (Century-Doomsday 2050) (Day-Of-Week 2))
(check-expect (Century-Doomsday 2199) (Day-Of-Week 0))
      
(: find-day-of-week (-> Date Day-Of-Week))
;; TAKEN FROM LAB2 - Lab Partner: Haichuan Wang
;; This is a helper function for day-layout. 
;; Takes in a Date (GivenDate) and uses its parameters to calculate which day 
;; of the week the date falls on. First the operation creates local definitions
;; to find out what the Doomsday for the year (Dooms-Year). Then the operator
;; calculates the difference between the specified day of GivenDate and the
;; Doomsday of the month provided by GivenDate (represented by the variable
;; "difference"). Finally, the operator takes this difference and, depending
;; on its value and sign, modifies it so it can be used to accurately
;; calculate the final day of the week. The operator returns the result as
;; a Day-Of-Week struct. 
;; 
(define (find-day-of-week GivenDate)
  (local
    {(define r (remainder (Date-y GivenDate) 100))
     (define add (+ r (exact-floor (/ r 4))))
     (define Dooms-Century (Century-Doomsday (Date-y GivenDate)))
     (define Dooms-Year (days-after Dooms-Century add))
     (define difference (- (Date-d GivenDate) 
                           (doomsday-in-month (Date-m GivenDate)
                                              (Date-y GivenDate))))}
    (cond
      [(< difference 0)  
       (cond
         [(< difference -7)
          (days-after Dooms-Year (+ (remainder difference -7) 7))]
         [else
          (days-after Dooms-Year (+ difference 7))])]
      [else (days-after Dooms-Year difference)])))

(check-expect (find-day-of-week (Date 1 28 2021)) (Day-Of-Week 4))
(check-expect (find-day-of-week (Date 2 29 1600)) (Day-Of-Week 2))
(check-expect (find-day-of-week (Date 1 31 2021)) (Day-Of-Week 0))
(check-expect (find-day-of-week (Date 1 31 2020)) (Day-Of-Week 5))
(check-expect (find-day-of-week (Date 4 2 2020)) (Day-Of-Week 4))
(check-expect (find-day-of-week (Date 4 6 2020)) (Day-Of-Week 1))
(check-expect (find-day-of-week (Date 5 1 1990)) (Day-Of-Week 2))
(check-expect (find-day-of-week (Date 5 23 1990)) (Day-Of-Week 3))
(check-expect (find-day-of-week (Date 12 25 1700)) (Day-Of-Week 6))

(: Memorial-Day : Integer Integer Integer -> Integer)
;; Memorial-Day is a helper function for row that uses three integers - the
;; first one being the last day of May and the other two being the Month and
;; Year, respectively. It then uses recursion and find-day-of-week to calculate
;; the day of the month for that year that Memorial-Day lies on (last Monday). 
;;
(define (Memorial-Day last-day m y)
  (if (= 1 (Day-Of-Week-num (find-day-of-week (Date m last-day y))))
      last-day
      (Memorial-Day (- last-day 1) m y)))

(check-expect (Memorial-Day 31 5 2020) 25)
(check-expect (Memorial-Day 31 5 2021) 31)
(check-expect (Memorial-Day 31 5 1997) 26)

(: Labor-Day : Integer Integer Integer -> Integer)
;; Labor-Day is a helper function for row that uses three integers - the
;; first one being the first day of Sept and the other two being the Month and
;; Year, respectively. It then uses recursion and find-day-of-week to calculate
;; the day of the month for that year that Labor-Day lies on (first Monday).
;;
(define (Labor-Day first-day m y)
  (if (= 1 (Day-Of-Week-num (find-day-of-week (Date m first-day y))))
      first-day
      (Labor-Day (+ first-day 1) m y)))

(check-expect (Labor-Day 1 9 2021) 6)
(check-expect (Labor-Day 1 9 2020) 7)
(check-expect (Labor-Day 1 9 2022) 5)

(: Thanksgiving-Day : Integer Integer Integer Integer -> Integer)
;; Thanksgiving-Day is a helper function for row that uses four integers - the
;; first one being the first day of Nov, the second is the counter used to find
;; the fourth Thursday, and the other two being the Month and Year, 
;; respectively. It then uses recursion and find-day-of-week to calculate
;; the day of the month for that year that Thanksgiving-Day lies on (fourth
;; Thursday).
;;
(define (Thanksgiving-Day first-day counter m y)
  (if (= 4 (Day-Of-Week-num (find-day-of-week (Date m first-day y))))
      (if (= counter 1)
          first-day
          (Thanksgiving-Day (+ first-day 1) (- counter 1) m y))
      (Thanksgiving-Day (+ first-day 1) counter m y)))

(check-expect (Thanksgiving-Day 1 4 11 2020) 26)
(check-expect (Thanksgiving-Day 1 4 11 2021) 25)
(check-expect (Thanksgiving-Day 1 4 11 2022) 24)

(: row : Integer Image-Color Integer Integer Integer Integer Integer -> Image)
;; row is a helper function for grid and it takes in CalFormat parameters
;; and another integer that represents the length of the row and returns
;; an image with respect to these variables.
;;
(define (row cell-size cell-fc total-days current-day length-left m y)
  (cond
    [(< length-left 1)
     (overlay
      (text (number->string current-day) (cast (* 1/2 cell-size) Byte)
            cell-fc)
      (square cell-size 'outline 'black)
      (cond 
        [(and (= m 5) (= current-day (Memorial-Day total-days m y))) 
         (square cell-size 'solid 'lavender)]
        [(and (= m 9) (= current-day (Labor-Day 1 m y))) 
          (square cell-size 'solid 'lime)]
        [(and (= m 11) (= current-day (Thanksgiving-Day 1 4 m y))) 
          (square cell-size 'solid 'orange)]
        [else empty-image]))]
    [else
     (beside
      (overlay
       (text (number->string current-day) (cast (* 1/2 cell-size) Byte)
             cell-fc)
       (square cell-size 'outline 'black)
       (cond 
         [(and (= m 5) (= current-day (Memorial-Day total-days m y))) 
         (square cell-size 'solid 'lavender)]
        [(and (= m 9) (= current-day (Labor-Day 1 m y))) 
          (square cell-size 'solid 'lime)]
        [(and (= m 11) (= current-day (Thanksgiving-Day 1 4 m y))) 
          (square cell-size 'solid 'orange)]
         [else empty-image]))
      (row cell-size cell-fc total-days (+ 1 current-day)
           (- length-left 1) m y))]))

(: grid : Integer Image-Color Integer Integer Day-Of-Week Integer
   Integer -> Image)
;; grid is a helper function for day-layout and takes in the cell size as an
;; integer, the font color, the total number of days in the month, and the
;; current day of the week that starts on the 1st. Using this, grid draws
;; the dates in calender format.
;;
(define (grid cell-size cell-fc total-days current-day which-day m y)
  (local
    {(define day-marker (Day-Of-Week-num which-day))
     (define this-row
       (row cell-size cell-fc total-days current-day
            (- 6 day-marker) m y))}
    (cond
      [(> current-day total-days) empty-image]
      [else (if (= current-day 1)
                (above 
                 (beside
                  (rectangle (* day-marker cell-size) cell-size 'outline
                             (color 0 0 0 0))
                  this-row)
                 (grid cell-size cell-fc total-days
                       (+ current-day (- 6 day-marker) 1) (Day-Of-Week 0) m y))
                (if (> (+ current-day 6) total-days)
                    (beside
                     (row cell-size cell-fc total-days current-day
                          (- total-days current-day) m y)
                     (rectangle (* (- 6 (- total-days current-day)) cell-size)
                                cell-size 'outline (color 0 0 0 0)))
                    (above
                     this-row
                     (grid cell-size cell-fc total-days
                           (+ current-day (- 6 day-marker) 1)
                           (Day-Of-Week 0) m y))))])))

(: day-layout : CalFormat Integer Integer -> Image)
;; day-layout is a helper function for draw-month that takes in a CalFormat 
;; and two integers (a month and year) and draws the arrangement of the days in 
;; a calendar format with respect to the month and year.
;;
(define (day-layout fmt m y)
  (match fmt
    ['() empty-image]
    [(CalFormat cell-size _ _ _ _ _ _ _ cell-fc)
     (grid cell-size cell-fc
           (days-in-month m y)
           1
           (find-day-of-week (Date m 1 y)) m y)]))

(: empty-grid-row : Integer Image-Color Integer -> Image)
;; empty-grid-row is a helper function for draw-month that takes in ColFormat
;; parameters and an integer for the length of the row and returns an image of a
;; row with the given parameters. 
;;
(define (empty-grid-row cell-size cell-c n)
  (if (> n 0)
      (beside
       (overlay
        (square cell-size 'solid cell-c)
        (square cell-size 'outline 'black))
       (empty-grid-row cell-size cell-c (- n 1)))
      empty-image))
   
(: draw-month : CalFormat Integer Integer -> Image)
;; draw-month takes in a CalFormat and one integer representing month and
;; another integer representing the year, respectively, and draws a calender
;; based on the month, year, and values provided in the CalFormat. draw-month
;; also provides visual indications of Memorial Day, Labor Day, and
;; Thanksgiving. 
;;
(define (draw-month fmt m y)
  (match fmt
    ['() empty-image]
    [(CalFormat cell-size _ _ _ _ _ _ cell-c _) 
     (above 
      (background-layout fmt m y)
      (local
        {(define empty-seven
           (overlay
            (rectangle (* cell-size 7) cell-size 'outline 'black)
            (empty-grid-row cell-size cell-c 7)))}
        (if (> (image-height (day-layout fmt m y))
               (* 5 cell-size))
            (overlay/align
             "middle" "top" 
             (day-layout fmt m y)
             (above 
              empty-seven
              (rectangle (* cell-size 7) (* cell-size 4) 'solid cell-c)
              empty-seven))
            (overlay/align
             "middle" "top" 
             (day-layout fmt m y)
             (above
              empty-seven
              (rectangle (* cell-size 7) (* cell-size 3) 'solid cell-c)
              empty-seven
              (rectangle (* cell-size 7) cell-size 'solid
                         (color 0 0 0 0)))))))]))

(: draw : CalWorld -> Image)
;; draw is a intermediate function that takes in a CalWorld and it's parameters
;; and feeds it into draw-month. 
(define (draw calworld1)
  (match calworld1
    ['() empty-image]
    [(CalWorld fmt m y) (draw-month fmt m y)]))

(draw-month fmt0 11 2021)
(draw-month fmt0 9 2021)
(draw-month fmt0 5 2021)

(: react-to-key : CalWorld String -> CalWorld)
;; react-to-key detects which key is pressed and if left or right arrow keys
;; are pressed then the calender moves up or down a month as needed. If up or
;; down is pressed then the calender moves up or down a year as needed. If
;; a month goes past Dec or before Jan, then adjust the year accordingly.
;;
(define (react-to-key calworld1 key)
  (match calworld1
    ['() empty-image]
    [(CalWorld fmt m y)
     (match key
       ["left" (if (= m 1)
                   (CalWorld fmt 12 (- y 1))
                   (CalWorld fmt (- m 1) y))]
       ["right" (if (= m 12)
                   (CalWorld fmt 1 (+ y 1))
                   (CalWorld fmt (+ m 1) y))]
       ["up" (CalWorld fmt m (+ y 1))]
       ["down" (CalWorld fmt m (- y 1))])]))
       
(: run : CalFormat Integer Integer -> CalWorld)
;; run creates a universe with the specified CalFormat and two integers: the
;; first representing the month and the second representing the year. run also
;; allows button-down key actions to be detected.
(define (run fmt m y)
  (big-bang (CalWorld fmt m y) : CalWorld
    [name "CS151 Project1 Winter 2021"]
    [to-draw draw]
    [on-key react-to-key]))

(test)