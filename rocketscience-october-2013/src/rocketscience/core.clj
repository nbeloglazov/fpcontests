(ns rocketscience.core
  (:require [clojure.data.json :as json]
            [rocketscience.segment-tree :refer :all]
            [cheshire.core :refer [generate-string]])
  (:gen-class))

(def tanks (into {}
  (for [tank (map #(zipmap [:name :m-full :m-empty :size] %)
                  [[ "FL-T100 Fuel Tank" 0.5625 0.0625 :small]
                   [ "FL-T200 Fuel Tank" 1.125 0.125 :small]
                   [ "FL-T400 Fuel Tank" 2.25 0.25 :small]
                   [ "FL-T800 Fuel Tank" 4.5 0.5 :small]
;                   [ "Rockomax X200-8 Fuel Tank" 4.5 0.5 :large]
                   [ "Rockomax X200-16 Fuel Tank" 9 1 :large]
                   [ "Rockomax X200-32 Fuel Tank" 18 2 :large]
                   [ "Rockomax Jumbo-64 Fuel Tank" 36 4 :large]])]
    [(:name tank) tank])))


(def engines (into {}
  (for [engine (map #(zipmap [:name :size :mass :thurst :isp-atm :isp-vac] %)
                    [[ "LV-T30 Liquid Fuel Engine" :small 1.25 215 320 370]
;                     [ "LV-T45 Liquid Fuel Engine" :small 1.5  200 320 370]
                     [ "LV-909 Liquid Fuel Engine" :small 0.5  50  300 390]
                     [ "Toroidal Aerospike Rocket" :small 1.5  175 388 390]
                     [ "Rockomax \"Poodle\" Liquid Engine"   :large 2.5 220  270 390]
                     [ "Rockomax \"Mainsail\" Liquid Engine" :large 6   1500 280 330]
                     [ "Rockomax \"Skipper\" Liquid Engine"  :large 4   650  300 350]
                     [ "LV-N Atomic Rocket Engine" :small 2.25 60  220 800 ]])]
    [(:name engine) engine])))

(defn mass-of-stage [stage full?]
  (let [{:keys [fuel engine central numSideParts numEngines height]} stage
        parts (+ numSideParts (if central 1 0))
        tank-weight (get-in tanks [fuel (if full? :m-full :m-empty)])]
    (+ (* parts height tank-weight)
       (* numEngines (get-in engines [engine :mass])))))

(defn type-of-stage [{:keys [numSideParts central]}]
  (cond (and central (zero? numSideParts)) :first
        central :second
        :else :third))

(defn mass-of-decoupler [[bottom top]]
  (cond (and (= :first (type-of-stage top))
             (= :third (type-of-stage bottom)))
        (* (:numSideParts bottom) 0.025)

        (and (= :large (get-in engines [(:engine top) :size]))
             (= :large (get-in tanks [(:fuel bottom) :size])))
        0.4

        :else 0.05))

(defn mass [stages full? payload]
  (let [decouplers (reduce + (map mass-of-decoupler (partition 2 1 stages)))
        bottom-stage (mass-of-stage (last stages) full?)
        other-stages (reduce +(map #(mass-of-stage % true) (butlast stages)))]
   (+ decouplers
      bottom-stage
      other-stages
      payload)))

(defn stage-dv
  ([stages payload first?]
     (let [m-start (mass stages true payload)
           m-end (mass stages false payload)
           engine (engines (:engine (last stages)))]
       (stage-dv m-start m-end engine first?)))
  ([m-start m-end engine first?]
     (let [isp (if first?
                 (:isp-atm engine)
                 (:isp-vac engine))]
       (* (Math/log (/ m-start m-end))
          isp 9.816))))

(defn acceleration [stages payload]
  (let [stage (last stages)
        engine (-> stage :engine engines)]
    (/ (* (:numEngines stage) (:thurst engine))
       (mass stages true payload))))

(defn dv [stages payload]
  (loop [sum 0
         stages stages
         first? true]
    (if (empty? stages)
      sum
      (let [acc (acceleration stages payload)]
        (if (or (and (< sum 5000)
                     (< acc 15))
                (< acc 5))
          nil
          (recur (+ sum (stage-dv stages payload first?))
                 (butlast stages)
                 false))))))

(defn spy
  ([v]
     (println v)
     v)
  ([text v]
     (println text v)
     v))

(def ^:dynamic *max-stages* 3)

(def ^:dynamic *best-dvs* nil)

(defn update-best [mass dv height]
  (if *best-dvs*
    (let [mass (int (Math/ceil mass))]
      (if (< (query-max (@*best-dvs* height) 0 mass) dv)
        (do (swap! *best-dvs* #(update-in % [height] update mass dv))
            true)
        false))
    true))

(defn total-height [stages]
  (apply + (map :height stages)))

(defn indent [stages]
  (apply str (repeat (count stages) "  ")))

(defn brute-force [max-mass payload need-dv stages]
;  (println (indent stages) max-mass)
;  (println (generate-string stages {:pretty true}))
  (cond (< max-mass payload) nil
        (if-let [d (dv stages payload)]
          (>= d need-dv)
          false) stages
         :else (let [got-min-dv (dv stages (dec max-mass))]
                (->> (for [central? [true false]
                           :when got-min-dv
                           :let [accel (if (< got-min-dv 5000) 15 5)]
                           engine (vals engines)
                           num-side-parts [0 2 3 4 6]
                           :when (or central? (pos? num-side-parts))
                           num-engines (cond (zero? num-side-parts) [1]
                                             (not central?) [num-side-parts]
                                             :else [1 (inc num-side-parts)])
                           :when (or (not= (:name engine) "Toroidal Aerospike Rocket")
                                     (= num-engines num-side-parts))
                           :let [max-mass (min max-mass (/ (* num-engines (:thurst engine))
                                                           accel))]
                           tank (vals tanks)
                           height (range 1 (inc (min 3 (- 7 (total-height stages)))))
                           :let [total-parts (+ num-side-parts (if central? 1 0))
                                 stage-mass (+ (* num-engines (:mass engine))
                                               (* height total-parts (:m-full tank)))
                                 stage {:engine (:name engine)
                                        :fuel (:name tank)
                                        :central central?
                                        :numSideParts num-side-parts
                                        :numEngines num-engines
                                        :height height}]
                           :when (<= (+ stage-mass payload) max-mass)
                           :let [sol (brute-force (- max-mass stage-mass) payload need-dv (cons stage stages))]
                           :when sol]
                       sol)
                     first))))

;(brute-force 1000 10 7000 [])

(defn find-stage
  ([max-mass payload dv]
     (binding [*max-stages* 7
               *best-dvs* (atom (vec (repeat 8 (create 0 (inc max-mass)))))]
       (find-stage max-mass 0 (+ payload 1) 7 dv 0 true 1)))
  ([max-mass current-mass payload allowed-height left-dv got-dv first? depth]
     (cond (< max-mass payload) nil
        (<= left-dv 0) []
        (<= allowed-height 0) nil
        (> depth *max-stages*) nil
        :else (let [expected-thurst (* max-mass (if (<= got-dv 5000) 15 5))
                    allowed-mass (- max-mass payload)]
                (first (for [; choose engines
                             engine (vals engines)

                             ; minimum number of engines to get necessary acceleration
                             :let [need-engines (quot (+ expected-thurst (:thurst engine) -1)
                                                      (:thurst engine))]

                             ; choose number of side parts
                             num-side-parts [0 2 3 4 6]

                             ; make sure we'll be able to attach necessary number of engines
                             :when (>= num-side-parts (dec need-engines))

                             ; choose if we have centarl part
                             central? [true false]

                             ; make sure we have either central part or side parts (stage is not empty)
                             :when (or central? (pos? num-side-parts))

                             ; choose number of engines
                             num-engines (cond (zero? num-side-parts) [1]
                                               (not central?) [num-side-parts]
                                               :else [1 (inc num-side-parts)])

                             ; make sure we choosed enough number of engines and we didn't choose Toroidal engine if we have central part
                             :when (and (>= num-engines need-engines)
                                        (or (not= (:name engine) "Toroidal Aerospike Rocket")
                                            (= num-engines num-side-parts)))

                             ; choose fuel tank
                             tank (vals tanks)

                             ; choose height
                             height ;[1 (min allowed-height 3)]
                                         (range 1 (inc (min allowed-height 3)))

                             ; calculate mass of the stage
                             :let [total-parts (+ num-side-parts (if central? 1 0))
                                   stage-mass (+ (* num-engines (:mass engine))
                                                 (* height total-parts (:m-full tank)))]

                             ; make sure the stage is not heavier than allowed
                             :when (<= stage-mass allowed-mass)

                             ; calculate dv of the stage and other stuff
                             :let [dv (stage-dv max-mass
                                                (- max-mass (* (- (:m-full tank) (:m-empty tank))
                                                               height total-parts))
                                                engine
                                                first?)
                                   stage {:engine (:name engine)
                                          :fuel (:name tank)
                                          :central central?
                                          :numSideParts num-side-parts
                                          :numEngines num-engines
                                          :height height}
                                   total-mass (+ current-mass stage-mass)
                                   total-dv (+ got-dv dv)]

                             ; check that we didn't have sitution earlier with smaller mass and better dv
                             :when (update-best total-mass total-dv (- allowed-height height))

                             ; calculate last stages
                             :let [next-stages (find-stage (- max-mass stage-mass)
                                                           total-mass
                                                           payload
                                                           (- allowed-height height)
                                                           (- left-dv dv)
                                                           total-dv
                                                           false
                                                           (inc depth))]

                             ; make sure we found configuration of last stages
                             ; and make sure we connect stages of third type to stages of first type
                             :when (and next-stages
                                        (or (not= :third (type-of-stage stage))
                                            (= :first (type-of-stage (last next-stages)))))]
                         (conj next-stages stage)))))))

(defn any-solution [payload dv time-left]
  (println "find any initial solution")
  (loop [mass 200]
    (cond (> mass 700)
          (println "mass can't be more than 700 tons. no solution found :(")

          (pos? (time-left))
          (do
            (printf "\ntime left %d sec\n" (int (time-left)))
            (println "trying mass " mass)
            (if-let [stages (find-stage mass payload dv)]
              stages
              (recur (+ mass 100))))

          :else
          (println "time is up. no solution found :("))))


(defn save-solution [sol payload]
  (println "\nfound solution with mass" (mass sol true payload) "and dv" (dv sol payload))
  (println "saved to solution.json\n")
  (spit "solution.json" (generate-string sol {:pretty true})))

(defn improve-solution [max-mass payload dv time-left]
  (loop [max-mass max-mass
         dx 10]
    (let [cur-mass (- max-mass dx)]
      (if (pos? (time-left))
        (do (printf "time left %d sec\n" (int (time-left)))
            (println "trying mass" cur-mass "\n")
            (if-let [sol (find-stage cur-mass payload dv)]
              (do (save-solution sol payload)
                  (recur cur-mass 10))
              (cond (= 1 dx) (recur max-mass 11)
                    (> dx 10) (recur max-mass (inc dx))
                    :else (recur max-mass (dec dx)))))
        (println "time is up, check solution.json")))))

;(find-stage 200 1.5 12000)

(defn run [payload dv timeout]
  (let [start (System/currentTimeMillis)
        end (+ start (* timeout 60 1000))
        time-left #(/ (- end (System/currentTimeMillis)) 1000.0)
        sol (any-solution payload dv time-left)]
    (when sol
      (save-solution sol payload)
      (improve-solution (int (mass sol true payload)) payload dv time-left))))


(defn -main [& [payload dv timeout]]
  (if dv
    (run (Double/parseDouble payload) (Double/parseDouble dv) (if timeout
                                                                (Double/parseDouble timeout)
                                                                2))
    (println "usage: lein run paylod dv [timeout]\n"
             "payload - mass need to deliver\n"
             "dv - delta V\n"
             "timeout - optional timeout for task in seconds. Default is 2.")))
