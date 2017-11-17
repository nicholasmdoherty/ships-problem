;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ships) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct ship [arrival unload])
; A Ship is a (make-ship N N)
; INTERPRETATION
; - arrival is the time when the ship docks
; - unload is the time it takes to unload

; Ships is a [List-of Ship]
; with ordered arrival times: 0 < n1 < n2 < n3 ...
; assume there are no gaps in arrival

(define ship1 (make-ship 0 5))
(define ship2 (make-ship 5 10))
(define ship3 (make-ship 15 7))
(define ship4 (make-ship 20 5))

(define ship5 (make-ship 0 6))
(define ship6 (make-ship 1 1))
(define ship7 (make-ship 2 1))
(define ship8 (make-ship 3 1))
(define ship9 (make-ship 4 1))


(define ships1 (list ship1 ship2 ship3))
(define ships2 (list ship1 ship2 ship3 ship4))
(define ships3 (list ship5 ship6 ship7 ship8 ship9))

(define avg-ships1 (/ 22 3))
(define avg-ships2 (/ 27 4))
(define avg-ships3 (/ 10 5))

; Ships -> Number
; Compute the expected average time spent in queue

(check-expect (average-time '()) 0)
(check-expect (average-time ships1) avg-ships1)
(check-expect (average-time ships2) avg-ships2)
(check-expect (average-time ships3) avg-ships3)

(define (average-time ships-0)
  (local [; Ships Ships N -> Number
          ; ships-in-harbor represents the ships we can unload
          ; ships-waiting are those still waiting to get into the harbor
          ; ACCUMULATOR elapsed time between when (first ship-0) arrived and
          ; when ships-in-harbor is empty
          
          ; 1. What does this data mean?
          ; -- Time that has passed
          
          ; 2. What is the initial value of the accumulator?
          ; -- 0, no time has passed
          
          ; 3. How do you update the accumulator for recursive calls?
          ; -- add the unload time of the unloaded ship to the accumulator
          
          ; 4. How can the function utilize the accumulator?
          ; -- divides the accumulator by the number of ships unloaded for the average time
          
          (define (wait/a ships-in-harbor ships-waiting clock)
            (cond
              [(empty? ships-in-harbor) (/ clock (length ships-0))]
              [else
               (local [; **IDEA**
                       ; 1. choose the ship from ship-in-harbor with the shortest unload time
                       ; 2. unload it by adding this time to the clock
                       ; 3. remove it from ships-in-harbor
                       ; 4. determine new ships that have arrived, ETA <= clock
                       ; 5. combine the lists from 3. and 4.
                       ; 6. remove those ships from ships-waiting
                       
                       ; 1. choose the ship from ship-in-harbor with the shortest unload time
                       (define next-ship-to-unload (argmin ship-unload ships-in-harbor))
               
                       ; 2.  unload it by adding this time to the clock
                       (define clock* (+ (ship-unload next-ship-to-unload) clock))

                       ; 3. remove it from ships-in-harbor
                       ; (all the ships in ships-in-harbor, expect for the one we are unloading)
                       (define all-except-unloaded (remove next-ship-to-unload ships-in-harbor))

                       ; Ship -> Boolean
                       ; Determines if the ETA of s is less than or equal to clock*
                       (define (ready-to-dock? s)
                         (<= (ship-arrival s) clock*))

                       ; 4. determine new ships that have arrived, ETA <= clock*
                       (define newly-arrived (filter ready-to-dock? ships-waiting))

                       ; 5. combines all-except-unloaded and newly-arrived
                       (define ships-in-harbor* (append all-except-unloaded newly-arrived))
               
                       ; 6. remove those ships from ships-waiting
                       (define ships-waiting* (foldr remove ships-waiting newly-arrived))]
                 (wait/a ships-in-harbor* ships-waiting* clock*))]))]
    (cond
      [(empty? ships-0) 0] ; If there are no ships, then the average wait time is 0
      [else (wait/a (list (first ships-0)) (rest ships-0) 0)]))) ; The first of ships-0 has already
                                                                 ; arrived so the rest are waiting



  