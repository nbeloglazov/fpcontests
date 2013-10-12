(ns rocketscience.core
  (:require [clojure.data.json :as json]))

(def tanks (into {}
  (for [tank (map #(zipmap [:name :m-full :m-empty :size] %)
                  [[ "FL-T100 Fuel Tank" 0.5625 0.0625 :small]
                   [ "FL-T200 Fuel Tank" 1.125 0.125 :small]
                   [ "FL-T400 Fuel Tank" 2.25 0.25 :small]
                   [ "FL-T800 Fuel Tank" 4.5 0.5 :small]
                   [ "Rockomax X200-8 Fuel Tank" 4.5 0.5 :large]
                   [ "Rockomax X200-16 Fuel Tank" 9 1 :large]
                   [ "Rockomax X200-32 Fuel Tank" 18 2 :large]
                   [ "Rockomax Jumbo-64 Fuel Tank" 36 4 :large]])]
    [(:name tank) tank])))


(def engines (into {}
  (for [engine (map #(zipmap [:name :size :mass :thurst :isp-atm :isp-vac] %)
                    [[ "LV-T30 Liquid Fuel Engine" :small 1.25 215 320 370]
                     [ "LV-T45 Liquid Fuel Engine" :small 1.5  200 320 370]
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

(defn stage-dv [stages payload first?]
  (let [m-start (mass stages true payload)
        m-end (mass stages false payload)
        engine (engines (:engine (last stages)))
        isp (if first?
              (:isp-atm engine)
              (:isp-vac engine))]
    (println m-start m-end isp)
    (* (Math/log (/ m-start m-end))
       isp 9.816)))

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

(def config (json/read-str (slurp "test.json") :key-fn keyword))

