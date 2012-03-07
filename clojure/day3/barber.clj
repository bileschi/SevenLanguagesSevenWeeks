; http://en.wikipedia.org/wiki/Sleeping_barber_problem

;;;;;;;;;;; Definition ;;;;;;;;;;;;;;;;;;;;;;

;A barber shop takes customers.
;Customers arrive at random intervals, from 100 to 300 milliseconds.
;The barber shop has three chairs in the waiting room.
;The barber shop has one barber and one barber chair.
;When the barber's chair is empty, a customer sits in the chair, wakes up the barber, and gets a haircut.
;If the chairs are occupied, all new customers will turn away.
;Haircuts take 200 milliseconds.
;After a customer receives a haircut, he gets up and leaves.
;barbershop is open for 

;;;;;;;;;;; Implementation ;;;;;;;;;;;;;;;;;;;

; defines whether the barber shop is open or not.  
; Customers will arrive while open, but will stop when the shop is closed
; the barber may continue to work in the closed shop if there is a queue
(def shop_is_open? (atom true))
; filled_seats is protected from concurrency issues via ref.
(def filled_seats (ref 0))
; filled seat limit doesn't need such protections since it does not change
(def filled_seat_limit 3)
; whether the barber is busy or not is maintained by this piece of state
; protected by the agent.
(def barber (agent 0))
; ensure that the thread generating customers does not duplicate ids
(def customer_id (atom 0))

; for printing out info to console.  Shmelessly cribbed from 
; http://www.bestinclass.dk/
; added graphical representation of people in seats
(defn debug
  [msg n]
  (println msg (apply str (repeat (- 25 (count msg)) \space)) n "   " (repeat @filled_seats "*"))
  (flush))

(defn cut_hair
	[cuts_so_far id]
	(dosync (alter filled_seats dec))
	(debug "(h) beginning cut for" id)
	(Thread/sleep 200)
	(debug "(h) cut complete" id)
	(inc cuts_so_far)
)

; if there is a seat available, the customer will take the seat
; and ask the barber to cut his hair.
(defn send_customer_to_shop
	[id]
	(dosync
		(if (>= @filled_seats filled_seat_limit)
			(debug "(c) leaving :(" id)
			(do 
				(alter filled_seats inc)
				(debug "(c) took a seat :)" id)
				(send-off barber cut_hair id)
			)
		)
	)
)

; generates a new customer id every 200-300ms.
(defn run_customer_generator
	[]
	(while @shop_is_open?
		(do 
			;(debug "(c) generated customer " @customer_id)
			(send_customer_to_shop @customer_id)
			(swap! customer_id inc)
			(Thread/sleep (+ 100 (rand-int 200)))
		)
	)
)

; in one thread, sets the shop open flag to true, waits, 
; then sets the flag false.  In a separate thread it kicks off the
; customer generator, which loops while the shop is open.
(defn run_shop
	[open_how_long]
	(debug "(!) opening shop..." "-")
	(reset! shop_is_open? true)
	(future
		(Thread/sleep open_how_long)
		(debug "(!) closing shop!" "-")
		(reset! shop_is_open? false)
	)
	(run_customer_generator)
	(debug "(!) total cuts" @barber)
)

(run_shop 8000)

