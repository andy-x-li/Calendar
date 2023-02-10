#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

(require typed/test-engine/racket-tests)
(require typed/racket/date)

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

(define-struct Time
  ([hour : Integer] ;; from 0 to 23
   [minute : Integer]
   [second : Integer]))

(define-struct Date
  ([month : Integer] ;; 1 for January, ..., 12 for December
   [day : Integer]
   [year : Integer]))

(define-struct CalWorld2
  ([mode : (U 'calendar 'help)]
   [format : CalFormat]
   [calendar-current-date : Date]
   [now-date : Date]
   [now-date-string : String]
   [now-time : Time]))

;; TAKEN FROM MY PROJECT 1 
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

;; TAKEN FROM MY PROJECT 1 
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

;; TAKEN FROM MY PROJECT 1 
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

;; TAKEN FROM MY PROJECT 1 
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

;; TAKEN FROM MY PROJECT 1 
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

;; TAKEN FROM MY PROJECT 1 
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

;; TAKEN FROM MY PROJECT 1 
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

;; TAKEN FROM MY PROJECT 1 
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

;; TAKEN FROM MY PROJECT 1 
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

;; TAKEN FROM MY PROJECT 1 
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
    {(define r (remainder (Date-year GivenDate) 100))
     (define add (+ r (exact-floor (/ r 4))))
     (define Dooms-Century (Century-Doomsday (Date-year GivenDate)))
     (define Dooms-Year (days-after Dooms-Century add))
     (define difference (- (Date-day GivenDate) 
                           (doomsday-in-month (Date-month GivenDate)
                                              (Date-year GivenDate))))}
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

;; TAKEN FROM MY PROJECT 1 
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

;; TAKEN FROM MY PROJECT 1 
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

;; TAKEN FROM MY PROJECT 1 
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

(: row : Integer Image-Color Integer Integer Integer Integer Integer Integer
   -> Image)
;; row is a helper function for grid and it takes in CalFormat parameters,
;; another integer that represents the length of the row, the month, the day,
;; and the year and returns a calendar row with respect to those parameters. 
;; row also draws any holidays and the calendar marker. 
;;
(define (row cell-size cell-fc total-days current-day length-left m d y)
  (cond
    [(< length-left 1)
     (overlay
      (text (number->string current-day) (cast (* 1/2 cell-size) Byte)
            cell-fc)
      (square cell-size 'outline 'black)
      (if (= current-day d)
          (overlay 
           (circle (cast (round (* 1/3 cell-size)) Byte) 'outline 'black)
           (circle (cast (round (* 1/3 cell-size)) Byte) 'solid 'yellow))
          empty-image)
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
       (if (= current-day d)
           (overlay 
            (circle (cast (round (* 1/3 cell-size)) Byte) 'outline 'black)
            (circle (cast (round (* 1/3 cell-size)) Byte) 'solid 'yellow))
          empty-image)
       (cond 
         [(and (= m 5) (= current-day (Memorial-Day total-days m y))) 
         (square cell-size 'solid 'lavender)]
        [(and (= m 9) (= current-day (Labor-Day 1 m y))) 
          (square cell-size 'solid 'lime)]
        [(and (= m 11) (= current-day (Thanksgiving-Day 1 4 m y))) 
          (square cell-size 'solid 'orange)]
         [else empty-image]))
      (row cell-size cell-fc total-days (+ 1 current-day)
           (- length-left 1) m d y))]))

(: grid : Integer Image-Color Integer Integer Day-Of-Week Integer
   Integer Integer -> Image)
;; grid is a helper function for day-layout and takes in the cell size as an
;; integer, the font color, the total number of days in the month, the
;; current day of the week that starts on the 1st, the Day-Of-Week of the first
;; day in the month, the month, the day, and the year. Using this, grid draws
;; the dates in calender format. It makes a call to row which checks for the
;; holidays and for the location of the dates along with the marker position. 
;;
(define (grid cell-size cell-fc total-days current-day which-day m d y)
  (local
    {(define day-marker (Day-Of-Week-num which-day))
     (define this-row
       (row cell-size cell-fc total-days current-day
            (- 6 day-marker) m d y))}
    (cond
      [(> current-day total-days) empty-image]
      [else (if (= current-day 1)
                (above 
                 (beside
                  (rectangle (* day-marker cell-size) cell-size 'outline
                             (color 0 0 0 0))
                  this-row)
                 (grid cell-size cell-fc total-days
                       (+ current-day (- 6 day-marker) 1) (Day-Of-Week 0)
                       m d y))
                (if (> (+ current-day 6) total-days)
                    (beside
                     (row cell-size cell-fc total-days current-day
                          (- total-days current-day) m d y)
                     (rectangle (* (- 6 (- total-days current-day)) cell-size)
                                cell-size 'outline (color 0 0 0 0)))
                    (above
                     this-row
                     (grid cell-size cell-fc total-days
                           (+ current-day (- 6 day-marker) 1)
                           (Day-Of-Week 0)
                           m d y))))])))

(: day-layout : CalFormat Integer Integer Integer -> Image)
;; day-layout is a helper function for draw-month that takes in a CalFormat 
;; and two integers (a month and year) and draws the arrangement of the days in 
;; a calendar format with respect to the month and year. It also makes a call to
;; grid which does the calculations for the positioning and current day marker. 
;;
(define (day-layout fmt m d y)
  (match fmt
    ['() empty-image]
    [(CalFormat cell-size _ _ _ _ _ _ _ cell-fc)
     (grid cell-size cell-fc
           (days-in-month m y)
           1
           (find-day-of-week (Date m 1 y)) m d y)]))

;; TAKEN FROM MY PROJECT 1 
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

(: draw-month : CalFormat Integer Integer Integer -> Image)
;; draw-month takes in a CalFormat and one integer representing month and
;; another integer representing the year, respectively, and draws a calendar
;; based on the month, year, and values provided in the CalFormat. draw-month
;; also provides visual indications of Memorial Day, Labor Day, and
;; Thanksgiving. It also makes a call to day-layout which does most of the
;; day formatting. 
;;
(define (draw-month fmt m d y)
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
          (if (> (image-height (day-layout fmt m d y))
                 (* 5 cell-size))
              (overlay/align
               "middle" "top" 
               (day-layout fmt m d y)
               (above 
                empty-seven
                (rectangle (* cell-size 7) (* cell-size 4) 'solid cell-c)
                empty-seven))
              (overlay/align
               "middle" "top"
               (day-layout fmt m d y)
               (above
                empty-seven
                (rectangle (* cell-size 7) (* cell-size 3) 'solid cell-c)
                empty-seven
                (rectangle (* cell-size 7) cell-size 'solid
                           (color 0 0 0 0)))))))]))

(: day-text : Integer -> String)
;; day-text takes in an integer and returns the day of the week that corrolates
;; to the integer. A helper function for draw tick and run. 
;;
(define (day-text n)
  (match n
    [0 "Sunday"]
    [1 "Monday"]
    [2 "Tuesday"]
    [3 "Wednesday"]
    [4 "Thursday"]
    [5 "Friday"]
    [6 "Saturday"]))

(check-expect (day-text 1) "Monday")
(check-expect (day-text 6) "Saturday")

(: yesterday : Date -> Date)
;; yesterday takes in a date and returns a date that is one day before on the
;; calendar.
;;
(define (yesterday date)
  (cond
    [(= 1 (Date-day date))
     (if (= 1 (Date-month date))  
         (Date 12 31 (- (Date-year date) 1))
         (Date (- (Date-month date) 1)
               (days-in-month (- (Date-month date) 1) (Date-year date))
               (Date-year date)))]
    [else (Date (Date-month date) (- (Date-day date) 1) (Date-year date))]))

(check-expect (yesterday (Date 1 1 2000)) (Date 12 31 1999))
(check-expect (yesterday (Date 2 1 2000)) (Date 1 31 2000))
(check-expect (yesterday (Date 3 1 2000)) (Date 2 29 2000))
(check-expect (yesterday (Date 3 15 2000)) (Date 3 14 2000))

(: tomorrow  : Date -> Date)
;; tomorrow takes in a date and returns a date that is one day after on the
;; calendar.
;;
(define (tomorrow date)
  (cond
    [(= (days-in-month (Date-month date) (Date-year date)) (Date-day date))
     (if (= 12 (Date-month date))
         (Date 1 1 (+ (Date-year date) 1))
         (Date (+ 1 (Date-month date)) 1 (Date-year date)))]
    [else (Date (Date-month date) (+ 1 (Date-day date)) (Date-year date))]))

(check-expect (tomorrow (Date 1 4 2000)) (Date 1 5 2000))
(check-expect (tomorrow (Date 1 31 2000)) (Date 2 1 2000))
(check-expect (tomorrow (Date 12 31 2000)) (Date 1 1 2001))
(check-expect (tomorrow (Date 11 30 2000)) (Date 12 1 2000))
(check-expect (tomorrow (Date 2 29 2000)) (Date 3 1 2000))

(: help-menu : CalFormat -> Image)
;; help-menu is a helper function for the draw function. It takes in a
;; CalFormat and returns the help menu as an Image.
;;
(define (help-menu fmt)
  (local
    {(define dimensions
       (cast (round (* 1/2 (CalFormat-cell-size fmt))) Byte))}
    (overlay/align
     "right" "top"
     (text "Press esc to return"
           (cast (round (* 2/5 (CalFormat-cell-size fmt))) Byte) 'black)
     (overlay
      (above
       (overlay
        (text "Help Menu" dimensions 'black)
        (square (* 2 dimensions) 'solid (color 0 0 0 0)))
       (overlay
        (above
         (beside 
          (overlay
           (text "+" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black))
          (overlay
           (text "+day" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black)))
         (beside
          (overlay
           (text "-" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black))
          (overlay
           (text "-day" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black)))
         (beside
          (overlay
           (text "[right]" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black))
          (overlay
           (text "+month" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black)))
         (beside
          (overlay
           (text "[left]" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black))
          (overlay
           (text "-month" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black)))
         (beside
          (overlay
           (text "[up]" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black))
          (overlay
           (text "+year" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black)))
         (beside
          (overlay
           (text "[down]" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black))
          (overlay
           (text "-year" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black)))
         (beside
          (overlay
           (text "T" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black))
          (overlay
           (text "today" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black)))
         (beside
          (overlay
           (text "?" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black))
          (overlay
           (text "help" dimensions 'black)
           (rectangle (* 8 dimensions) (* 2 dimensions) 'outline 'black))))
        (rectangle (* 8 (cast (CalFormat-cell-size fmt) Byte))
                   (* 8 (cast (CalFormat-cell-size fmt) Byte)) 'outline
                   'black)))
      (rectangle (* 14 (cast (CalFormat-cell-size fmt) Byte))
                 (* 9 (cast (CalFormat-cell-size fmt) Byte)) 'solid 'white)))))

(: draw : CalWorld2 -> Image)
;; the draw function outputs an Image of the background with the real life 
;; date and time which it also calculates. It determines whether to draw the 
;; calendar or the help menu based on its CalWorld2 parameters.
;;
(define (draw world)
  (match world
    ['() empty-image]
    [(CalWorld2 mode fmt cal-current real-date real-date-string real-time)
     (if (symbol=? mode 'help)
         (help-menu fmt)
         (underlay/align
          "right" "top"
          (underlay/align
           "left" "bottom" 
           (overlay/align
            "left" "top"
            (draw-month fmt (Date-month cal-current)
                        (Date-day cal-current)
                        (Date-year cal-current))
            (rectangle (* 14 (CalFormat-cell-size fmt))
                       (* 9 (CalFormat-cell-size fmt)) 'solid 'white))
           (text (string-append
                  (day-text (Day-Of-Week-num (find-day-of-week cal-current)))
                  ", "
                  (month-text (Date-month cal-current))
                  " "
                  (number->string (Date-day cal-current))
                  ", "
                  (number->string (Date-year cal-current)))
                 (cast (* 1/2 (CalFormat-cell-size fmt)) Byte)
                 (CalFormat-cell-font fmt)))
          (above/align
           "right" 
           (text real-date-string
                 (cast (round (* 2/5 (CalFormat-cell-size fmt))) Byte)
                 'black)
           (rectangle 0 5 'solid 'black)
           (text (string-append
                  (number->string (cond
                                    [(and (>= (Time-hour real-time) 0)
                                          (< (Time-hour real-time) 1))
                                     (+ 12 (Time-hour real-time))]
                                    [(< (Time-hour real-time) 13)
                                     (Time-hour real-time)]
                                    [else (- (Time-hour real-time) 12)]))
                  ":"
                  (if (< (Time-minute real-time) 10)
                      (string-append "0" (number->string (Time-minute
                                                          real-time)))
                      (number->string (Time-minute real-time)))
                  ":"
                  (if (< (Time-second real-time) 10)
                      (string-append "0" (number->string (Time-second
                                                          real-time)))
                      (number->string (Time-second real-time)))
                  " "
                  (if (< (Time-hour real-time) 12)
                      "am"
                      "pm"))
                 (cast (round (* 2/5 (CalFormat-cell-size fmt))) Byte)
                 'black)
           (rectangle 0 5 'solid 'black)
           (text "Press ? for help"
                 (cast (round (* 2/5 (CalFormat-cell-size fmt))) Byte)
                 'black))))]))
             
(: react-to-key : CalWorld2 String -> CalWorld2)
;; react-to-key detects which key is pressed and reacts accordingly. If the "T"
;; key is pressed, the function returns a CalWorld2 with the current calendar
;; date reset to the real world date. If the "+" or "-" key is pressed the
;; function returns a CalWorld with the current calendar day pushed one day
;; forward or backwards, respectively. The arrow keys control the month and year
;; in a similar way with "left" and "right" controlling the months and "up" and
;; "down" controlling the years. Lastly, the "?" key brings up the help menu
;; while the "escape" key returns to the calendar screen. 
;;
(define (react-to-key world key)
  (match world
    ['() world]
    [(CalWorld2 mode fmt cal-current real-date real-date-string real-time)
     (match key
       ["T" (CalWorld2 mode fmt real-date real-date real-date-string real-time)]
       ["+" (CalWorld2 mode fmt (tomorrow cal-current)
                       real-date real-date-string real-time)]
       ["-" (CalWorld2 mode fmt (yesterday cal-current)
                       real-date real-date-string real-time)]
       ["left" (local
                 {(define last-month-total
                    (days-in-month
                     (if (= (Date-month cal-current) 1)
                         12
                         (- (Date-month cal-current) 1))
                     (if (= (Date-month cal-current) 1)
                         (- (Date-year cal-current) 1)             
                         (Date-year cal-current))))}
                 (if (= (Date-month cal-current) 1)
                     (CalWorld2 mode
                                fmt          
                                (Date 12
                                      (if (> (Date-day cal-current) 
                                             last-month-total)
                                          last-month-total
                                          (Date-day cal-current))
                                      (- (Date-year cal-current) 1))
                                real-date
                                real-date-string
                                real-time)
                     (CalWorld2 mode
                                fmt
                                (Date (- (Date-month cal-current) 1)
                                      (if (> (Date-day cal-current) 
                                             last-month-total)
                                          last-month-total
                                          (Date-day cal-current))
                                      (Date-year cal-current))
                                real-date
                                real-date-string
                                real-time)))]
       ["right" (local
                   {(define next-month-total
                    (days-in-month
                     (if (= (Date-month cal-current) 12)
                         1
                         (+ (Date-month cal-current) 1))
                     (if (= (Date-month cal-current) 12)
                         (+ (Date-year cal-current) 1)             
                         (Date-year cal-current))))}
                  (if (= (Date-month cal-current) 12)
                      (CalWorld2 mode
                                 fmt
                                 (Date 1
                                       (if (> (Date-day cal-current) 
                                              next-month-total)
                                           next-month-total
                                           (Date-day cal-current))
                                        (+ (Date-year cal-current) 1))
                                 real-date
                                 real-date-string
                                 real-time)
                      (CalWorld2 mode
                                 fmt
                                 (Date (+ (Date-month cal-current) 1)
                                       (if (> (Date-day cal-current) 
                                              next-month-total)
                                           next-month-total
                                           (Date-day cal-current))
                                        (Date-year cal-current))
                                 real-date
                                 real-date-string
                                 real-time)))]
       ["up" (CalWorld2 mode
                        fmt
                        (Date 
                         (if (and (= 2 (Date-month cal-current))
                                  (= 29 (Date-day cal-current)))
                             3
                             (Date-month cal-current))
                         (if (and (= 2 (Date-month cal-current))
                                  (= 29 (Date-day cal-current)))
                             1
                             (Date-day cal-current))

                         (+ 1 (Date-year cal-current)))
                        real-date
                        real-date-string
                        real-time)]
        ["down" (CalWorld2 mode
                           fmt
                           (Date 
                            (if (and (= 2 (Date-month cal-current))
                                  (= 29 (Date-day cal-current)))
                                3
                                (Date-month cal-current))
                            (if (and (= 2 (Date-month cal-current))
                                  (= 29 (Date-day cal-current)))
                                1
                                (Date-day cal-current))

                         (- (Date-year cal-current) 1))
                        real-date
                        real-date-string
                        real-time)]
       ["?" (CalWorld2 'help
                       fmt cal-current real-date real-date-string real-time)]
       ["escape" (CalWorld2 'calendar
                            fmt cal-current real-date real-date-string
                            real-time)]
       [_ world])]))

(: read-date-now : -> Date)
;; read-date-now returns the current date at the moment as a Date struct. It is
;; helper function for run.
;;
(define (read-date-now)
  (Date (date-month (current-date))
        (date-day (current-date))
        (date-year (current-date))))

(: read-time-now : -> Time)
;; read-time-now returns the current time at the moment as a Time struct. It is
;; helper function for run.
;;
(define (read-time-now)
  (Time (date-hour (current-date))
        (date-minute (current-date))
        (date-second (current-date))))

(: tick : CalWorld2 -> CalWorld2)
;; tick function is used to update the date and time
;;
(define (tick world)
  (match world
    ['() world]
    [(CalWorld2 mode fmt cal-current real-date real-date-string real-time)
     (CalWorld2 mode fmt cal-current (if (and (= (Time-hour real-time) 0)
                                              (= (Time-minute real-time) 0))
                                         (read-date-now)
                                         real-date)
                (if (and (= (Time-hour real-time) 0)
                         (= (Time-minute real-time) 0))
                    (string-append 
                     (day-text
                      (Day-Of-Week-num (find-day-of-week (read-date-now))))
                     ", "
                     (month-text (Date-month (read-date-now)))
                     " "
                     (number->string (Date-day (read-date-now)))
                     ", "
                     (number->string (Date-year (read-date-now))))
                    real-date-string)
                (read-time-now))]))          

(: run : CalFormat Integer Integer -> CalWorld2)
;; run creates a universe with the specified CalFormat and two integers: the
;; first representing the month and the second representing the year. run calls
;; the draw, tick, and react-to-key functions as well. run also creates the
;; a CalWorld2 with these three values and the computer's built-in clock.  
;;
(define (run fmt m y)
  (big-bang (CalWorld2
             'calendar
             fmt
             (Date m 1 y)
             (read-date-now)
             (string-append 
              (day-text (Day-Of-Week-num (find-day-of-week (read-date-now))))
              ", "
              (month-text (Date-month (read-date-now)))
              " "
              (number->string (Date-day (read-date-now)))
              ", "
              (number->string (Date-year (read-date-now))))
             (read-time-now)) : CalWorld2
    [name "CS151 Project1 Winter 2021"]
    [to-draw draw]
    [on-tick tick 1/4]
    [on-key react-to-key]))


(test)