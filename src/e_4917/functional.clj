(ns e-4917.functional
  (:import 
    [javax.swing JFrame JPanel JButton JTextArea]
    [java.awt.event ActionListener]
    [java.util Date]))

(defstruct state :program :r0 :r1 :ip :is)

(def output (JTextArea. 10 50))

(defn add 
  "r0 = r0 + r1"
  [{:keys [program r0 r1 ip is]}]
  (struct state program (+ r0 r1) r1 (inc ip) 1))
  
(defn subtract 
  "r0 = r0 - r1"
  [{:keys [program r0 r1 ip is]}]
  (struct state program (- r0 r1) r1 (inc ip) 2))

(defn inc-r0 
  "r0 = r0 + 1"
  [{:keys [program r0 r1 ip is]}]
  (struct state program (inc r0) r1 (inc ip) 3))

(defn inc-r1 
  "r1 = r1 + 1"
  [{:keys [program r0 r1 ip is]}]
  (struct state program r0 (inc r1) (inc ip) 4))
  
(defn dec-r0 
  "r0 = r0 - 1"
  [{:keys [program r0 r1 ip is]}]
  (struct state program (dec r0) r1 (inc ip) 5))
  
(defn dec-r1 
  "r1 = r1 - 1"
  [{:keys [program r0 r1 ip is]}]
  (struct state program r0 (dec r1) (inc ip) 6))
  
(defn bell
  "Ring a bell"
  [{:keys [program r0 r1 ip is]}]
  (do 
    (.append output "buzz\n")
    (struct state program r0 r1 (inc ip) 7)))
  
(defn prnt 
  "Print value in next memory location"
  [{:keys [program r0 r1 ip is]}]
  (do
    (.append output (str (nth program (inc ip)) "\n"))
    (struct state program r0 r1 (+ 2 ip) 8)))
  
(defn load-r0 
  "Load value at addres into r0"
  [{:keys [program r0 r1 ip is]}]
  (struct state program (nth program (nth program (inc ip))) r1 (+ 2 ip) 9))

(defn load-r1
  "Load value at addres into r1"
  [{:keys [program r0 r1 ip is]}]
  (struct state program r0 (nth program (nth program (inc ip))) (+ 2 ip) 10))
  
(defn store-r0 
  "Store r0 into address n"
  [{:keys [program r0 r1 ip is]}]
  (struct state (assoc program (nth program (inc ip)) r0) r0 r1 (+ 2 ip) 11)) 
  
(defn store-r1 
  "Store r1 into address n"
  [{:keys [program r0 r1 ip is]}]
  (struct state (assoc program (nth program (inc ip)) r1) r0 r1 (+ 2 ip) 12))
  
(defn jump 
  "Jump to n"
  [{:keys [program r0 r1 ip is]}]
  (struct state program  r0 r1 (nth program (inc ip)) 13))

(defn jump-if-zero
  "Jump to address <data> if R0 == 0"
  [{:keys [program r0 r1 ip is] :as state}]
  (if (zero? r0)
    (jump state)
    (struct state program r0 r1 (+ 2 ip) 14)))
  
(defn jump-if-not-zero
  "Jump to address <data> if R0 != 0"
  [{:keys [program r0 r1 ip is] :as state}]
  (if (not (zero? r0))
    (jump state)
    (struct state program r0 r1 (+ 2 ip) 15)))

(defn step
  "Takes a program, a r0, r1 an instruction pointer and a instruction store. And runs one step"
  [{:keys [program r0 r1 ip is] :as state}]
  (let [cur (try (nth program ip) (catch Exception _ 0))]
    (if (not (zero? cur))
      (cond 
        (= cur 1) (add state)
        (= cur 2) (subtract state)
        (= cur 3) (inc-r0 state)
        (= cur 4) (inc-r1 state)
        (= cur 5) (dec-r0 state)
        (= cur 6) (dec-r1 state)
        (= cur 7) (bell state)
        (= cur 8) (prnt state)
        ;; 2 byte instrunctions
        (= cur 9)  (load-r0 state)
        (= cur 10) (load-r1 state)
        (= cur 11) (store-r0 state)
        (= cur 12) (store-r1 state)
        (= cur 13) (jump state)
        (= cur 14) (jump-if-zero state)
        (= cur 15) (jump-if-not-zero state))
      false)))
  
(defn run
  "Takes a program, a r0, r1 an instruction pointer and a instruction store. And the hole program until instruction zero then it returns false."
  [{:keys [program r0 r1 ip is] :as state}]
  (let [stp (step state)]
    (if (false? stp)
      "Program is done"
      (recur stp))))

(defn gui
  []
  (let [frame   (JFrame. "4917 Emulator")
        panel   (JPanel.)
        stp     (JButton. "Step")
        rn      (JButton. "Run")
        load    (JButton. "Load")
        input   (JTextArea. 10 50)
        prog    (ref (struct state [8 7 7 7] 0 0 0 0))]
    
    (.addActionListener load
      (proxy [ActionListener] []
        (actionPerformed [e]
          (let [code (vec (map #(Integer/parseInt %)(re-seq #"\w+" (.getText input))))]
            (dosync
              (.setText output (str "\nProgram loaded: " (Date.) "\n"))
              (ref-set prog (struct state code 0 0 0 0)))))))
   
    (.addActionListener stp
      (proxy [ActionListener] []
       (actionPerformed [e] 
         (dosync
           (.append output (str "ip:" (:ip @prog) " r0: " (:r0 @prog) " r1: " (:r1 @prog) "\n"))
           (ref-set prog (step @prog))))))
         
    (.addActionListener rn
      (proxy [ActionListener] []
        (actionPerformed [e]
          (do
            (.setText output "")
            (run @prog)
            (.append output (str "\nProgram completed: " (Date.)))))))
   
   (doto panel
     (.add load)
     (.add stp)
     (.add rn)
     (.add input)
     (.add output))
     
   (doto frame
     (.add panel)
     (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
     (.setSize 610 500)
     (.setResizable false)
     (.setVisible true))))
                     