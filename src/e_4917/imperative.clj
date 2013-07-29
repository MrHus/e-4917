(ns e-4917.imperative)

(def ip (ref 0)) ; Instruction Pointer
(def is (ref 0)) ; Instruction Store  
(def r0 (ref 0)) ; Register Zero
(def r1 (ref 0)) ; Register One

(def program (ref [9, 63, 10, 62, 2, 11, 8, 8,
                   0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0, 0, 0, 0, 13, 12]))

(defn set-ref!
  [r v]
  (dosync
    (ref-set r v)))

(defn alter-ref!
  [r f]
  (dosync
    (alter r f)))  

(defn add 
	"r0 = r0 + r1" 
	[] 
(set-ref! r0 (+ @r0 @r1))) 

(defn subtract 
	"r0 = r0 - r1" 
	[] 
	(set-ref! r0 (- @r0 @r1))) 

(defn inc-r0 
	"r0 = r0 + 1"
	[] 
	(alter-ref! r0 inc))

(defn inc-r1 
	"r1 = r1 + 1" 
	[] 
	(alter-ref! r1 inc))
  
(defn dec-r0 
	"r0 = r0 - 1" 
	[] 
	(alter-ref! r0 dec))

(defn dec-r1 
	"r1 = r1 - 1" 
	[] 
(alter-ref! r1 dec))
  
(defn bell
  "Ring a bell"
  []
  (println "buzz"))

(defn prnt
  "Print value in next memory location"
  []
  (println (nth @program (inc @ip))))
  
(defn load-r0 "Load value at addres into r0" [n] (set-ref! r0 (nth @program n)))    
(defn load-r1 "Load value at addres into r1" [n] (set-ref! r1 (nth @program n)))
    
(defn store-r0 "Store R0 into address n" [n] (set-ref! program (assoc @program n @r0)))
(defn store-r1 "Store R1 into address n" [n] (set-ref! program (assoc @program n @r1)))

(defn jump "Jump to n" [n] (set-ref! ip (- n 2)))  ; Its n - 2 because of the increase by 2 (jump is 2 byte) of the Instruction Pointer in the run function.
  
(defn jump-if-zero
  "Jump to address <data> if R0 == 0"
  [n]
  (if (zero? @r0)
    (jump n)))
  
(defn jump-if-not-zero
  "Jump to address <data> if R0 != 0"
  [n]
  (if (not (zero? @r0))
    (jump n)))

(defn get-next-loc
  "Get the value from the memory cell next to the current instruction pointer"
  []
  (nth @program (inc @ip)))

(defn update-ip
  "Increase ip by 1 if opcode < 8 else increase by 2"
  [v]
  (if (< v 8)
    (alter-ref! ip inc)
    (set-ref! ip (+ @ip 2))))

(defn router
  "Take a number and execute accordingly"
  [n]
  (cond 
    (= n 1) (add)
    (= n 2) (subtract)
    (= n 3) (inc-r0)
    (= n 4) (inc-r1)
    (= n 5) (dec-r0)
    (= n 6) (dec-r1)
    (= n 7) (bell)    
    (= n 8) (prnt)
    ;; 2 byte instrunctions
    (= n 9)  (load-r0 (get-next-loc))
    (= n 10) (load-r1 (get-next-loc))
    (= n 11) (store-r0 (get-next-loc))
    (= n 12) (store-r1 (get-next-loc))
    (= n 13) (jump (get-next-loc))
    (= n 14) (jump-if-zero (get-next-loc))
    (= n 15) (jump-if-not-zero (get-next-loc))))
    
(defn run
  "Run the program until memory locatio is zero"
  []
  (let [v (nth @program @ip)]
    (if (not (zero? v))
      (do
        (println "Executing command: " v " ip: " @ip " r0: " @r0 " r1: " @r1)
        (router v)
        (update-ip v)
        (run))
        "Program Complete")))