#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

(require typed/test-engine/racket-tests)
(require typed/racket/date)

;; Name: Andy Li

(define-type (Optional A)
  (U 'None (Some A)))

(define-struct (Some A)
  ([value : A]))

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

(define-struct Time
  ([hour : Integer] ;; from 0 to 23
   [minute : Integer]
   [second : Integer]))

(define-struct Date
  ([month : Integer] ;; 1 for January, ..., 12 for December
   [day : Integer]
   [year : Integer]))

(define-struct Span
  ([start : Time]
   [end : Time]))

(define-struct Event
  ([date : Date]
   [time : (U 'all-day Time Span)]
   [description : String]))

(define-type EventTree
  (U 'Empty EventNode))

(define-struct EventNode
  ([date : Date]
   [events : (Listof Event)] ;; maintain this list in ascending order
   [lsub : EventTree]
   [rsub : EventTree]))

(define-struct CalWorld3
  ([mode : (U 'calendar 'help 'entry)]
   [entry-mode : (U 'start 'end 'description)]
   [format : CalFormat]
   [calendar-current-date : Date]
   [now-date : Date]
   [now-date-string : String]
   [now-time : Time]
   [notepad : String]
   [opt-start : (Optional Time)]
   [opt-end : (Optional Time)]
   [events : EventTree]))

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

(: fmt1 CalFormat)
;; fmt1 is an example format for testing purposes
;;
(define fmt1
  (CalFormat 80
             'dodgerblue 'navy 60
             'silver 'blue 40
             'lightblue 'black))

;; tree-example1 is a EventTree used for testing 
(define tree-example1
  (EventNode (Date 1 31 2020) (list
                               (Event (Date 1 31 2020) 'all-day "A")
                               (Event (Date 1 31 2020) (Time 2 2 0) "A")
                               (Event (Date 1 31 2020) (Time 2 2 0) "C")
                               (Event (Date 1 31 2020) (Span
                                                        (Time 3 1 0)
                                                        (Time 3 5 0)) "D"))
             (EventNode (Date 1 30 2020)
                        (list
                         (Event (Date 1 30 2020) 'all-day "A")
                         (Event (Date 1 30 2020) 'all-day "C")
                         (Event (Date 1 30 2020) (Time 2 2 0) "A")
                         (Event (Date 1 30 2020) (Span
                                                  (Time 3 1 0)
                                                  (Time 3 5 0)) "D"))
                        'Empty 'Empty)
             (EventNode (Date 2 1 2020)
                        (list
                         (Event (Date 2 1 2020) (Span
                                                 (Time 3 1 0)
                                                 (Time 3 5 0)) "D"))
                        'Empty 'Empty)))  

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
    [12 31]
    [_ (error "month provided out of range")]))
       
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

;; TAKEN FROM MY PROJECT 2
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

;; TAKEN FROM MY PROJECT 2
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

;; TAKEN FROM MY PROJECT 2
(: day-layout : CalFormat Integer Integer Integer -> Image)
;; day-layout is a helper function for draw-month that takes in a CalFormat 
;; and two integers (a month and year) and draws the arrangement of the days in
;; a calendar format with respect to the month and year. It also calls 
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
;; parameters and an integer for the length of the row and returns an image of 
;; a row with the given parameters. 
;;
(define (empty-grid-row cell-size cell-c n)
  (if (> n 0)
      (beside
       (overlay
        (square cell-size 'solid cell-c)
        (square cell-size 'outline 'black))
       (empty-grid-row cell-size cell-c (- n 1)))
      empty-image))

;; TAKEN FROM MY PROJECT 2
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

;; TAKEN FROM MY PROJECT 2
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

;; TAKEN FROM MY PROJECT 2
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

;; TAKEN FROM MY PROJECT 2
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

;; TAKEN FROM MY PROJECT 2
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




(: string->time : String -> (Optional Time))
;; convert a string like "11:25am" to a Time struct, if possible
;; ex: (string->time "10:00am") -> (Some (Time 10 0 0))
;; ex: (string->time "4:30pm")  -> (Some (Time 16 30 0))
;; ex: (string->time "abcde")   -> 'None
(define (string->time str1)
  (local
    {(define str-list (string->list str1))}
    (cond
      [(= 7 (length str-list))
       (if (and (char=? #\1 (first str-list))
                (char-numeric? (second str-list))
                (< -1 (cast
                       (string->number (list->string (list (second str-list))))
                       Byte))
                (> 3 (cast
                      (string->number (list->string (list (second str-list))))
                      Byte))
                (char=? #\: (third str-list))
                (char-numeric? (fourth str-list))
                (and (< -1 (cast
                            (string->number (list->string
                                             (list (fourth str-list))))
                            Byte))
                     (> 6 (cast
                           (string->number (list->string
                                            (list (fourth str-list))))
                           Byte)))
                (char-numeric? (fifth str-list))
                (or (char=? #\a (sixth str-list))
                    (char=? #\p (sixth str-list)))
                (char=? #\m (seventh str-list)))
           (if (char=? #\a (sixth str-list))
               (Some (Time (cast
                            (string->number
                             (list->string (append (list (first str-list))
                                                   (list (second str-list)))))
                            Byte)
                           (cast
                            (string->number
                             (if (= 0 (cast
                                       (string->number
                                        (list->string
                                         (list (fourth str-list))))
                                       Byte))
                                 (list->string (list (fifth str-list)))
                                 (list->string (append
                                                (list (fourth str-list))
                                                (list (fifth str-list))))))
                            Byte)
                           0))
               (Some (Time (+ 12 
                              (cast
                               (string->number
                                (list->string (append
                                               (list (first str-list))
                                               (list (second str-list)))))
                               Byte))
                           (cast
                            (string->number
                             (if (= 0 (cast
                                       (string->number
                                        (list->string
                                         (list (fourth str-list))))
                                       Byte))
                                 (list->string (list (fifth str-list)))
                                 (list->string (append
                                                (list (fourth str-list))
                                                (list (fifth str-list))))))
                            Byte)
                           0)))
           'None)]
      [(= 6 (length str-list))
       (if (and (char-numeric? (first str-list))
                (< 0 (cast
                      (string->number (list->string (list (first str-list))))
                      Byte))
                (char=? #\: (second str-list))
                (char-numeric? (third str-list))
                (and (< -1 (cast
                            (string->number (list->string
                                             (list (third str-list))))
                            Byte))
                     (> 6 (cast
                           (string->number (list->string
                                            (list (third str-list))))
                           Byte)))
                (char-numeric? (fourth str-list))
                (or (char=? #\a (fifth str-list))
                    (char=? #\p (fifth str-list)))
                (char=? #\m (sixth str-list)))
           (if (char=? #\a (fifth str-list))
               (Some (Time (cast
                            (string->number
                             (list->string (list (first str-list))))
                            Byte)
                           (cast
                            (string->number
                             (if (= 0 (cast
                                       (string->number
                                        (list->string
                                         (list (third str-list))))
                                       Byte))
                                 (list->string (list (fourth str-list)))
                                 (list->string (append
                                                (list (third str-list))
                                                (list (fourth str-list))))))
                            Byte)
                           0))
               (Some (Time (+ 12 
                              (cast
                               (string->number
                                (list->string (list (first str-list))))      
                                Byte))
                           (cast
                            (string->number
                             (if (= 0 (cast
                                       (string->number
                                        (list->string
                                         (list (third str-list))))
                                       Byte))
                                 (list->string (list (fourth str-list)))
                                 (list->string (append
                                                (list (third str-list))
                                                (list (fourth str-list))))))
                            Byte)
                           0)))
           'None)]
      [else 'None])))
               
(check-expect (string->time "12:45am") (Some (Time 12 45 0)))
(check-expect (string->time "11:05am") (Some (Time 11 5 0)))
(check-expect (string->time "13:45am") 'None)
(check-expect (string->time "10:50pm") (Some (Time 22 50 0)))
(check-expect (string->time "10:62am") 'None)
(check-expect (string->time "12:09pm") (Some (Time 24 9 0)))
(check-expect (string->time "12:00pm") (Some (Time 24 0 0)))
(check-expect (string->time "00:09pm") 'None)
(check-expect (string->time "1:45am") (Some (Time 1 45 0)))
(check-expect (string->time "9:05am") (Some (Time 9 5 0)))
(check-expect (string->time "9:50pm") (Some (Time 21 50 0)))
(check-expect (string->time "4:00pm") (Some (Time 16 0 0)))
(check-expect (string->time "0:34pm")'None)
(check-expect (string->time "12:43abcd") 'None)
(check-expect (string->time "2:43abcd") 'None)
(check-expect (string->time "abcdef") 'None)
(check-expect (string->time "ghijklm") 'None)

(: is-before? : Time Time -> (Optional Boolean))
;; helper function that determines if the first time comes before the second
;; time does in a day. returns 'None for tiebreakers. 
(define (is-before? t1 t2)
  (match* (t1 t2)
    [((Time h1 m1 s1) (Time h2 m2 s2))
     (cond
       [(< h1 h2) (Some #t)]
       [(> h1 h2) (Some #f)]
       [else
        (cond
          [(< m1 m2) (Some #t)]
          [(> m1 m2) (Some #f)]
          [else 'None])])]))

(check-expect (is-before? (Time 2 2 0) (Time 2 2 0)) 'None)
(check-expect (is-before? (Time 2 1 0) (Time 2 2 0)) (Some #t))
(check-expect (is-before? (Time 1 2 0) (Time 2 2 0)) (Some #t))
(check-expect (is-before? (Time 2 3 0) (Time 2 2 0)) (Some #f))
(check-expect (is-before? (Time 3 2 0) (Time 2 2 0)) (Some #f))


(: event<? : Event Event -> Boolean)
;; determines if the first event is "less than" the second according to event
;; order.
(define (event<? event1 event2)
  (match* ((Event-time event1) (Event-time event2))
    [('all-day 'all-day)
     (string<?
      (Event-description event1)
      (Event-description event2))]
    [('all-day (Span start end)) #t]
    [((Span start end) 'all-day) #f]
    [('all-day (Time _ _ _)) #t]
    [((Time _ _ _) 'all-day) #f]
    [((Time h1 m1 s1) (Time h2 m2 s2))
     (match (is-before? (Time h1 m1 s1) (Time h2 m2 s2))
       ['None (string<? (Event-description event1) (Event-description event2))]
       [(Some #t) #t]
       [(Some #f) #f])]
    [((Time h1 m1 s1) (Span (Time h2 m2 s2) _))
     (match (is-before? (Time h1 m1 s1) (Time h2 m2 s2))
       ['None (string<? (Event-description event1) (Event-description event2))]
       [(Some #t) #t]
       [(Some #f) #f])]
    [((Span (Time h2 m2 s2) _) (Time h1 m1 s1))
     (match (is-before? (Time h2 m2 s2) (Time h1 m1 s1))
       ['None (string<? (Event-description event1) (Event-description event2))]
       [(Some #t) #t]
       [(Some #f) #f])]

    [((Span start1 end1) (Span start2 end2))
     (match (is-before? start1 start2)
       ['None (match (is-before? end1 end2)
                ['None (string<?
                        (Event-description event1) (Event-description event2))]
                [(Some #t) #t]
                [(Some #f) #f])]
       [(Some #t) #t]
       [(Some #f) #f])]))
(check-expect (event<?
               (Event (Date 2 2 2021) 'all-day "A")
               (Event (Date 2 2 2021) 'all-day "B")) #t)
(check-expect (event<?
               (Event (Date 2 2 2021) 'all-day "B")
               (Event (Date 2 2 2021) 'all-day "A")) #f)
(check-expect (event<?
               (Event (Date 2 2 2021) (Time 2 2 0) "A")
               (Event (Date 2 2 2021) 'all-day "A")) #f)
(check-expect (event<?
               (Event (Date 2 2 2021) 'all-day "A")
               (Event (Date 2 2 2021) (Time 2 2 0) "A")) #t)
(check-expect (event<?
               (Event (Date 2 2 2021) 'all-day "A")
               (Event (Date 2 2 2021) (Span (Time 2 2 0)
                                            (Time 2 3 0)) "A")) #t)
(check-expect (event<?
               (Event (Date 2 2 2021) (Span (Time 2 2 0)
                                            (Time 2 3 0)) "A")
               (Event (Date 2 2 2021) 'all-day "A")) #f)
(check-expect (event<?
               (Event (Date 2 2 2021) (Time 2 2 0) "A")
               (Event (Date 2 2 2021) (Time 2 3 0) "A")) #t)
(check-expect (event<?
               (Event (Date 2 2 2021) (Time 2 2 0) "A")
               (Event (Date 2 2 2021) (Time 2 1 0) "A")) #f)
(check-expect (event<?
               (Event (Date 2 2 2021) (Time 2 2 0) "A")
               (Event (Date 2 2 2021) (Time 2 2 0) "B")) #t)
(check-expect (event<?
               (Event (Date 2 2 2021) (Time 2 2 0) "B")
               (Event (Date 2 2 2021) (Time 2 2 0) "A")) #f)
(check-expect (event<?
               (Event (Date 2 2 2021) (Span (Time 2 2 0)
                                            (Time 2 3 0)) "A")
               (Event (Date 2 2 2021) (Span (Time 2 3 0)
                                            (Time 2 4 0)) "A")) #t)
(check-expect (event<?
               (Event (Date 2 2 2021) (Span (Time 2 2 0)
                                            (Time 2 3 0)) "A")
               (Event (Date 2 2 2021) (Span (Time 2 2 0)
                                            (Time 2 4 0)) "A")) #t)
(check-expect (event<?
               (Event (Date 2 2 2021) (Span (Time 2 2 0)
                                            (Time 2 3 0)) "A")
               (Event (Date 2 2 2021) (Span (Time 2 2 0)
                                            (Time 2 3 0)) "B")) #t)
(check-expect (event<?
               (Event (Date 2 2 2021) (Span (Time 2 2 0)
                                            (Time 2 3 0)) "B")
               (Event (Date 2 2 2021) (Span (Time 2 2 0)
                                            (Time 2 3 0)) "A")) #f)
(check-expect (event<?
               (Event (Date 2 2 2021) (Span (Time 2 2 0)
                                            (Time 2 4 0)) "A")
               (Event (Date 2 2 2021) (Span (Time 2 2 0)
                                            (Time 2 3 0)) "A")) #f)
(check-expect (event<?
               (Event (Date 2 2 2021) (Span (Time 2 2 0)
                                            (Time 2 3 0)) "A")
               (Event (Date 2 2 2021) (Span (Time 2 1 0)
                                            (Time 2 3 0)) "A")) #f)

;; TAKEN FROM LAB2 - Lab Partner: Haichuan Wang
(: date=? (-> Date Date Boolean))
;; helper function for insert-event-tree
;; Checks if the first inputed Date (Date1) is identical to the second Date
;; (Date2). If so, this operation returns #t. Otherwise, it returns #f. Months,
;; days, and years have to match exactly to be considered identical.
;;
(define (date=? Date1 Date2)
  (and (= (Date-month Date1) (Date-month Date2))
       (= (Date-day Date1) (Date-day Date2))
       (= (Date-year Date1) (Date-year Date2))))

(check-expect (date=? (Date 2 29 2000) (Date 2 29 2000)) #t)
(check-expect (date=? (Date 2 28 2000) (Date 2 29 2000)) #f)
(check-expect (date=? (Date 2 29 1999) (Date 2 29 2000)) #f)
(check-expect (date=? (Date 1 29 2000) (Date 2 29 2000)) #f)

;; TAKEN FROM LAB2 - Lab Partner: Haichuan Wang
(: date<? (-> Date Date Boolean))
;; helper function for insert-event-tree
;; Checks if the first inputed Date (Date1) comes before the second Date
;; (Date2). If it does, this operation returns #t. Otherwise, it returns #f. 
;;
(define (date<? Date1 Date2)
  (cond
    [(< (Date-year Date1) (Date-year Date2)) #t]
    [(and (= (Date-year Date1) (Date-year Date2))
          (< (Date-month Date1) (Date-month Date2))) #t]
    [(and (= (Date-year Date1) (Date-year Date2))
          (= (Date-month Date1) (Date-month Date2))
          (< (Date-day Date1) (Date-day Date2))) #t]
    [else #f]))

(check-expect (date<? (Date 2 20 1999) (Date 2 20 2000)) #t)
(check-expect (date<? (Date 1 20 2000) (Date 2 20 2000)) #t)
(check-expect (date<? (Date 2 19 2000) (Date 2 20 2000)) #t)
(check-expect (date<? (Date 2 20 2001) (Date 2 20 2000)) #f)
(check-expect (date<? (Date 3 20 2000) (Date 2 20 2000)) #f)
(check-expect (date<? (Date 2 21 2000) (Date 2 20 2000)) #f)
(check-expect (date<? (Date 2 20 2000) (Date 2 20 2000)) #f)

(: insert-event-list : Event (Listof Event) -> (Listof Event))
;; insert-event-list inserts an event in a list of events using event<?
;; to compare events and order them in an ascending order.
;; a helper function for insert-event-tree
(define (insert-event-list event1 event-list)
  (match event-list
    ['() (list event1)]
    [(cons head tail)
     (if (event<? event1 head)
         (cons event1 event-list)
         (cons head (insert-event-list event1 tail)))]))
        
(: insert-event-tree : Event EventTree -> EventTree)
;; insert an event into an event tree, creating the node if needed
;; note: no duplicate check is necessary; insert the event no matter what
;; note: maintain the events list in ascending event order
(define (insert-event-tree event1 event-tree)
  (match event-tree
    ['Empty (EventNode (Event-date event1) (list event1) 'Empty 'Empty)]
    [(EventNode date events lsub rsub)
     (cond
       [(date=? date (Event-date event1))
        (EventNode date (insert-event-list event1 events) lsub rsub)]
       [(date<? date (Event-date event1))
        (EventNode date events lsub (insert-event-tree event1 rsub))]
       [else (EventNode
              date events (insert-event-tree event1 lsub) rsub)])]))

(check-expect (insert-event-tree (Event (Date 1 31 2020) 'all-day "B")
                                 tree-example1)
              (EventNode (Date 1 31 2020)
                         (list
                          (Event (Date 1 31 2020) 'all-day "A")
                          (Event (Date 1 31 2020) 'all-day "B")
                          (Event (Date 1 31 2020) (Time 2 2 0) "A")
                          (Event (Date 1 31 2020) (Time 2 2 0) "C")
                          (Event (Date 1 31 2020) (Span
                                                   (Time 3 1 0)
                                                   (Time 3 5 0)) "D"))
                         (EventNode (Date 1 30 2020)
                                    (list
                                     (Event (Date 1 30 2020) 'all-day "A")
                                     (Event (Date 1 30 2020) 'all-day "C")
                                     (Event (Date 1 30 2020) (Time 2 2 0) "A")
                                     (Event (Date 1 30 2020)
                                            (Span
                                             (Time 3 1 0)
                                             (Time 3 5 0)) "D"))
                                    'Empty 'Empty)
                         (EventNode (Date 2 1 2020)
                                    (list
                                     (Event (Date 2 1 2020)
                                            (Span
                                             (Time 3 1 0)
                                             (Time 3 5 0)) "D"))
                                    'Empty 'Empty)))
(check-expect (insert-event-tree (Event (Date 1 31 2020) (Time 2 2 0) "B")
                                 tree-example1)
              (EventNode (Date 1 31 2020)
                         (list
                          (Event (Date 1 31 2020) 'all-day "A")
                          (Event (Date 1 31 2020) (Time 2 2 0) "A")
                          (Event (Date 1 31 2020) (Time 2 2 0) "B")
                          (Event (Date 1 31 2020) (Time 2 2 0) "C")
                          (Event (Date 1 31 2020) (Span
                                                   (Time 3 1 0)
                                                   (Time 3 5 0)) "D"))
                         (EventNode (Date 1 30 2020)
                                    (list
                                     (Event (Date 1 30 2020) 'all-day "A")
                                     (Event (Date 1 30 2020) 'all-day "C")
                                     (Event (Date 1 30 2020) (Time 2 2 0) "A")
                                     (Event (Date 1 30 2020)
                                            (Span
                                             (Time 3 1 0)
                                             (Time 3 5 0)) "D"))
                                    'Empty 'Empty)
                         (EventNode (Date 2 1 2020)
                                    (list
                                     (Event (Date 2 1 2020)
                                            (Span
                                             (Time 3 1 0)
                                             (Time 3 5 0)) "D"))
                                    'Empty 'Empty)))
(check-expect (insert-event-tree (Event (Date 1 30 2020)
                                            (Span
                                             (Time 3 1 0)
                                             (Time 3 5 0)) "C")
                                 tree-example1)
              (EventNode (Date 1 31 2020)
                         (list
                          (Event (Date 1 31 2020) 'all-day "A")
                          (Event (Date 1 31 2020) (Time 2 2 0) "A")
                          (Event (Date 1 31 2020) (Time 2 2 0) "C")
                          (Event (Date 1 31 2020) (Span
                                                   (Time 3 1 0)
                                                   (Time 3 5 0)) "D"))
                         (EventNode (Date 1 30 2020)
                                    (list
                                     (Event (Date 1 30 2020) 'all-day "A")
                                     (Event (Date 1 30 2020) 'all-day "C")
                                     (Event (Date 1 30 2020) (Time 2 2 0) "A")
                                     (Event (Date 1 30 2020)
                                            (Span
                                             (Time 3 1 0)
                                             (Time 3 5 0)) "C")
                                     (Event (Date 1 30 2020)
                                            (Span
                                             (Time 3 1 0)
                                             (Time 3 5 0)) "D"))
                                    'Empty 'Empty)
                         (EventNode (Date 2 1 2020)
                                    (list
                                     (Event (Date 2 1 2020)
                                            (Span
                                             (Time 3 1 0)
                                             (Time 3 5 0)) "D"))
                                    'Empty 'Empty)))
(check-expect (insert-event-tree (Event (Date 2 2 2020) 'all-day "B")
                                 tree-example1)
              (EventNode (Date 1 31 2020)
                         (list
                          (Event (Date 1 31 2020) 'all-day "A")
                          (Event (Date 1 31 2020) (Time 2 2 0) "A")
                          (Event (Date 1 31 2020) (Time 2 2 0) "C")
                          (Event (Date 1 31 2020) (Span
                                                   (Time 3 1 0)
                                                   (Time 3 5 0)) "D"))
                         (EventNode (Date 1 30 2020)
                                    (list
                                     (Event (Date 1 30 2020) 'all-day "A")
                                     (Event (Date 1 30 2020) 'all-day "C")
                                     (Event (Date 1 30 2020) (Time 2 2 0) "A")
                                     (Event (Date 1 30 2020)
                                            (Span
                                             (Time 3 1 0)
                                             (Time 3 5 0)) "D"))
                                    'Empty 'Empty)
                         (EventNode (Date 2 1 2020)
                                    (list
                                     (Event (Date 2 1 2020)
                                            (Span
                                             (Time 3 1 0)
                                             (Time 3 5 0)) "D"))
                                    'Empty
                                    (EventNode (Date 2 2 2020)
                                               (list
                                                (Event (Date 2 2 2020) 'all-day
                                                       "B"))
                                               'Empty 'Empty))))

(: insert-event-world : Event CalWorld3 -> CalWorld3)
;; insert an event into the event tree in a cal world
(define (insert-event-world event1 world)
  (match world
    [(CalWorld3 mode entry fmt cal-current real-date real-date-string real-time
                notepad start end events)
     (CalWorld3 mode entry fmt cal-current real-date real-date-string real-time
                notepad start end (insert-event-tree event1 events))]
    [_ world]))

(: retrieve-events : Date EventTree -> (Listof Event))
;; fetch the list of events for the given date
;; return empty list if that date is not present in the tree
(define (retrieve-events date1 tree1)
  (match tree1
    ['Empty '()]
    [(EventNode date events lsub rsub)
     (cond
       [(date=? date date1)
        events]
       [(date<? date date1)
        (retrieve-events date1 lsub)]
       [else
        (retrieve-events date1 rsub)])]))
     
(check-expect (retrieve-events (Date 2 2 2020) (EventNode
                                                (Date 2 2 2020)
                                                (list
                                                 (Event (Date 2 2 2020)
                                                        'all-day
                                                        "test"))
                                                'Empty 'Empty))
              (list (Event (Date 2 2 2020) 'all-day "test")))
                                                        

(: react-to-key : CalWorld3 String -> CalWorld3)
;; react-to-key detects which key is pressed and reacts accordingly. If the "T"
;; key is pressed, the function returns a CalWorld3 with the current calendar
;; date reset to the real world date. If the "+" or "-" key is pressed the
;; function returns a CalWorld with the current calendar day pushed one day
;; forward or backwards, respectively. The arrow keys control the month and year
;; in a similar way with "left" and "right" controlling the months and "up" and
;; "down" controlling the years. The "?" key brings up the help menu
;; while the "escape" key returns to the calendar screen. The return key brings
;; up an interface for adding calendar events for given times or just reminders.
;;
(define (react-to-key world key)
  (match world
    ['() world]
    [(CalWorld3 mode entry fmt cal-current real-date real-date-string real-time
                notepad start end events)
     (if (or (symbol=? mode 'calendar)
             (symbol=? mode 'help))
         (match key
           ["T" (CalWorld3 mode entry fmt real-date real-date real-date-string
                           real-time notepad start end events)]
           ["+" (CalWorld3 mode entry fmt (tomorrow cal-current) real-date 
                           real-date-string real-time notepad start end events)]
           ["-" (CalWorld3 mode entry fmt (yesterday cal-current) real-date
                           real-date-string real-time notepad start end events)]
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
                         (CalWorld3 mode
                                    entry
                                    fmt          
                                    (Date 12
                                          (if (> (Date-day cal-current) 
                                                 last-month-total)
                                              last-month-total
                                              (Date-day cal-current))
                                          (- (Date-year cal-current) 1))
                                    real-date real-date-string real-time notepad
                                    start end events)
                         (CalWorld3 mode
                                    entry
                                    fmt
                                    (Date (- (Date-month cal-current) 1)
                                          (if (> (Date-day cal-current) 
                                                 last-month-total)
                                              last-month-total
                                              (Date-day cal-current))
                                          (Date-year cal-current))
                                    real-date real-date-string real-time notepad
                                    start end events)))]
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
                          (CalWorld3 mode
                                     entry
                                     fmt
                                     (Date 1
                                           (if (> (Date-day cal-current) 
                                                  next-month-total)
                                               next-month-total
                                               (Date-day cal-current))
                                           (+ (Date-year cal-current) 1))
                                     real-date real-date-string real-time
                                     notepad start end events) 
                          (CalWorld3 mode
                                     entry
                                     fmt
                                     (Date (+ (Date-month cal-current) 1)
                                           (if (> (Date-day cal-current) 
                                                  next-month-total)
                                               next-month-total
                                               (Date-day cal-current))
                                           (Date-year cal-current))
                                     real-date real-date-string real-time 
                                     notepad start end events)))]
           ["up" (CalWorld3 mode
                            entry
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
                            real-date real-date-string real-time notepad
                            start end events)]
           ["down" (CalWorld3 mode
                              entry
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
                              real-date real-date-string real-time notepad
                              start end events)]
           ["?" (CalWorld3 'help
                           entry fmt cal-current real-date real-date-string
                           real-time notepad start end events)]
           ["\r" (CalWorld3 'entry entry fmt cal-current real-date
                            real-date-string real-time notepad start
                            end events)]
           ["escape" (CalWorld3 'calendar
                                'start fmt cal-current real-date
                                real-date-string
                                real-time "" start end events)]
           [_ world])
         (match key
           ["escape" (CalWorld3 'calendar
                                'start fmt cal-current real-date
                                real-date-string
                                real-time "" start end events)]
           ["\r" (if (symbol=? mode 'entry)
                     (cond
                       [(and (symbol=? entry 'start)
                             (string=? notepad ""))
                        (CalWorld3 'entry 'description fmt cal-current real-date
                                   real-date-string real-time notepad 'None
                                   'None events)]
                       [(and (symbol=? entry 'start)
                             (not (symbol=? 'None (match (string->time notepad)
                                                    ['None 'None]
                                                    [(Some A) 'a]))))
                        (CalWorld3 'entry 'end fmt cal-current real-date
                                   real-date-string real-time ""
                                   (string->time notepad)
                                   'None events)]
                       [(and (symbol=? entry 'end)
                             (string=? notepad ""))
                        (CalWorld3 'entry 'description fmt cal-current real-date
                                   real-date-string real-time "" start 'None
                                   events)]
                       [(and (symbol=? entry 'end)
                             (not (symbol=? 'None (match (string->time notepad)
                                                    ['None 'None]
                                                    [(Some A) 'a]))))
                        (CalWorld3 'entry 'description fmt cal-current real-date
                                   real-date-string real-time ""
                                   start (string->time notepad) events)]
                       [(symbol=? entry 'description)
                        (match* (start end)
                          [('None 'None)
                           (insert-event-world
                            (Event cal-current 'all-day notepad)
                            (CalWorld3 'calendar 'start fmt cal-current
                                       real-date real-date-string real-time ""
                                       start end events))]
                          [((Some (Time h1 m1 s1)) 'None)
                           (insert-event-world
                            (Event cal-current (Time h1 m1 s1) notepad)
                            (CalWorld3 'calendar 'start fmt cal-current
                                       real-date real-date-string real-time ""
                                       start end events))]
                          [((Some (Time h1 m1 s1)) (Some (Time h2 m2 s2)))
                           (insert-event-world
                            (Event cal-current (Span (Time h1 m1 s1)
                                                     (Time h2 m2 s2)) notepad)
                            (CalWorld3 'calendar 'start fmt cal-current
                                       real-date real-date-string real-time ""
                                       start end events))])]
                       [else world])
                     world)]
           ["`" (if (symbol=? mode 'entry)
                    (match entry
                      ['start 
                       (CalWorld3 'entry 'start fmt cal-current real-date
                                  real-date-string real-time ""
                                  start end events)]
                      ['end
                       (CalWorld3 'entry 'end fmt cal-current real-date
                                  real-date-string real-time ""
                                  start end events)]
                      ['description
                       (CalWorld3 'entry 'description fmt cal-current real-date
                                  real-date-string real-time ""
                                  start end events)])
                         world)]
                             
           [key (if (and (symbol=? mode 'entry)
                         (= (string-length key) 1))
                    (CalWorld3 'entry entry fmt cal-current real-date
                            real-date-string real-time
                            (string-append notepad key) start end events)
                    (CalWorld3 'entry entry fmt cal-current real-date
                               real-date-string real-time notepad start end
                               events))]))]))

(: event-interface : Symbol String (Optional Time) (Optional Time) EventTree
   Integer Date -> Image)
;; takes in parameters needed for the event input interface and draws it
;; visually in a way intuitive to the user.
;;
(define (event-interface entry notepad start end events cell-size cal-current)
  (local
    {(define space-box
       (square (cast (* 1/2 cell-size) Byte) 'outline (color 0 0 0 0)))}
    (overlay/align
     "left" "top"
     (above/align
      "left"
      (text "enter start time ([esc] to leave, ` to cancel)"
            (cast (* 1/2 cell-size) Byte) 'black)
      space-box
      (text (string-append
             (day-text (Day-Of-Week-num (find-day-of-week cal-current)))
             ", "
             (month-text (Date-month cal-current))
             " "
             (number->string (Date-day cal-current))
             ", "
             (number->string (Date-year cal-current)))
            (cast (* 1/2 cell-size) Byte)
            'black)
      space-box
      (match entry
        ['start (above/align
                 "left"
                 (overlay
                  (text notepad (cast (* 1/2 cell-size) Byte) 'black)
                  (rectangle (* 6 cell-size) cell-size 'outline 'black)
                  (rectangle (* 6 cell-size) cell-size 'solid 'lightskyblue))
                 space-box
                 (overlay
                  (rectangle (* 6 cell-size) cell-size 'outline 'black)
                  (rectangle (* 6 cell-size) cell-size 'solid 'lightskyblue))
                 space-box
                 (overlay
                  (rectangle (* 9 cell-size) cell-size 'outline 'black)
                  (rectangle (* 9 cell-size) cell-size 'solid 'lightskyblue)))]
        ['end (above/align
               "left"
               (rectangle (* 6 cell-size) cell-size 'solid
                          (color 220 220 220 100))
               space-box
               (overlay
                (text notepad (cast (* 1/2 cell-size) Byte) 'black)
                (rectangle (* 6 cell-size) cell-size 'outline 'black)
                (rectangle (* 6 cell-size) cell-size 'solid 'lightskyblue))
               space-box
               (overlay
                (rectangle (* 9 cell-size) cell-size 'outline 'black)
                (rectangle (* 9 cell-size) cell-size 'solid 'lightskyblue)))]
        ['description (above/align
                       "left"
                       (rectangle (* 6 cell-size) cell-size 'solid
                                  (color 220 220 220 100))
                       space-box
                       (rectangle (* 6 cell-size) cell-size 'solid
                                  (color 220 220 220 100))
                       space-box
                       (overlay
                        (text notepad (cast (* 1/2 cell-size) Byte) 'black)
                        (rectangle (* 9 cell-size) cell-size 'outline 'black)
                        (rectangle (* 9 cell-size) cell-size 'solid
                                   'lightskyblue)))]))
     (rectangle (* 14 cell-size) (* 9 cell-size) 'solid 'white))))

(: time->string : Time -> String)
;; takes in a time based on a 24 hour clock and outpts a 12 hour clock time as
;; a string. 
(define (time->string t1)
  (string-append
   (number->string (cond
                     [(and (>= (Time-hour t1) 0)
                           (< (Time-hour t1) 1))
                      (+ 12 (Time-hour t1))]
                     [(< (Time-hour t1) 13)
                      (Time-hour t1)]
                     [else (- (Time-hour t1) 12)]))
   ":"
   (if (< (Time-minute t1) 10)
       (string-append "0" (number->string (Time-minute t1)))
       (number->string (Time-minute t1)))
   (if (< (Time-hour t1) 12)
       "am"
       "pm")))

;; TAKEN FROM MY PROJECT 2
(: draw : CalWorld3 -> Image)
;; the draw function outputs an Image of the background with the real life 
;; date and time which it also calculates. It determines whether to draw the 
;; calendar or the help menu based on its CalWorld3 parameters.
;;
(define (draw world)
  (match world
    ['() empty-image]
    [(CalWorld3 mode entry fmt cal-current real-date real-date-string real-time
                notepad start end events)
     (cond
       [(symbol=? mode 'entry)
        (event-interface entry notepad start end events
                         (CalFormat-cell-size fmt) cal-current)]
       [(symbol=? mode 'help)
        (help-menu fmt)]
       [else
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
          (text "Press ? for help, [return] to enter event"
                (cast (round (* 2/5 (CalFormat-cell-size fmt))) Byte)
                'black)
          (overlay 
           (rectangle (* 6 (CalFormat-cell-size fmt)) 3 'solid 'lightskyblue)
           (rectangle 0 12 'solid 'blue))
          (match (retrieve-events cal-current events)
            ['() (overlay
                  (text
                   "-- no events --"
                   (cast (round (* 1/3 (CalFormat-cell-size fmt))) Byte)
                   'black)
                  (rectangle (* 6 (CalFormat-cell-size fmt)) 0
                             'solid 'blue))]
            [_ empty-image])
          (local
            {(: on-top : (Listof Image) -> Image)
             (define (on-top images)
               (foldr above empty-image images))    
             (: appointment : Event -> Image)
             (define (appointment event1)
               (match event1
                 [(Event date type description)
                  (above/align
                   "right"
                  (beside
                   (overlay 
                    (rectangle 3
                               (CalFormat-cell-size fmt)
                               'solid 'lightskyblue)
                    (rectangle 12 0 'solid 'blue))
                   (overlay/align
                    "left" "middle"
                    (above/align
                     "left"
                     (text (string-append
                            (day-text (Day-Of-Week-num (find-day-of-week
                                                        cal-current)))
                            ", "
                            (month-text (Date-month cal-current))
                            " "
                            (number->string (Date-day cal-current))
                            ", "
                            (number->string (Date-year cal-current)))
                           (cast (round (* 1/3 (CalFormat-cell-size fmt))) Byte)
                           'black)
                     (square 3 'solid (color 0 0 0 0))
                     (text 
                      (match type
                        ['all-day "(all-day)"]
                        [(Time _ _ _) (time->string type)]
                        [(Span t1 t2) (string-append
                                       (time->string t1)
                                       "-"
                                       (time->string t2))])
                      (cast (round (* 1/3 (CalFormat-cell-size fmt))) Byte)
                      'black)
                     (square 3 'solid (color 0 0 0 0))
                     (text description
                           (cast (round (* 1/3 (CalFormat-cell-size fmt))) Byte)
                           'black))
                    (rectangle
                     (* 6 (CalFormat-cell-size fmt))
                     (CalFormat-cell-size fmt)
                     'solid (color 0 0 0 0))))
                  (overlay 
                    (rectangle (* 6 (CalFormat-cell-size fmt)) 3 'solid
                               'lightskyblue)
                    (rectangle 0 12 'solid 'blue)))]))}
                   
            (on-top (map appointment
                         (retrieve-events cal-current events))))))])]))
                  
;; TAKEN FROM MY PROJECT 2
(: read-date-now : -> Date)
;; read-date-now returns the current date at the moment as a Date struct. It is
;; helper function for run.
;;
(define (read-date-now)
  (Date (date-month (current-date))
        (date-day (current-date))
        (date-year (current-date))))

;; TAKEN FROM MY PROJECT 2
(: read-time-now : -> Time)
;; read-time-now returns the current time at the moment as a Time struct. It is
;; helper function for run.
;;
(define (read-time-now)
  (Time (date-hour (current-date))
        (date-minute (current-date))
        (date-second (current-date))))

;; TAKEN FROM MY PROJECT 2
(: tick : CalWorld3 -> CalWorld3)
;; tick function is used to update the date and time
;;
(define (tick world)
  (match world
    ['() world]
    [(CalWorld3 mode entry fmt cal-current real-date real-date-string real-time
                notepad start end events)
     (CalWorld3 mode entry fmt cal-current (if (and (= (Time-hour real-time) 0)
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
                (read-time-now)
                notepad start end events)]))          

(: run : CalFormat Integer Integer -> CalWorld3)
;; run creates a universe with the specified CalFormat and two integers: the
;; first representing the month and the second representing the year. run calls
;; the draw, tick, and react-to-key functions as well. run also creates the
;; a CalWorld3 with these three values and the computer's built-in clock.  
;;
(define (run fmt m y)
  (big-bang (CalWorld3
             'calendar
             'start
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
             (read-time-now)
             ""
             'None
             'None
             'Empty) : CalWorld3
    [name "CS151 Project1 Winter 2021"]
    [to-draw draw]
    [on-tick tick 1/4]
    [on-key react-to-key]))

(test)