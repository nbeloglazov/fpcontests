(ns rocketscience.core
  (:require [rocketscience.segment-tree :refer :all]
            [cheshire.core :refer [generate-string]]
            [clojure.tools.logging :refer [info debug]])
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

(def number-of-workers (.availableProcessors (Runtime/getRuntime))
  )

(def task-queue (java.util.concurrent.LinkedBlockingQueue.))
(def result-queue (java.util.concurrent.LinkedBlockingQueue.))

(defn start-worker [id]
  (debug "worker" id "waiting for task")
  (let [task (.take task-queue)]
    (if-not (= task :die)
      (let [{:keys [time payload dv max-mass]} task]
        (debug "worker" id "calculating for mass" max-mass)
        (let [sol (find-stage max-mass payload dv)]
          (.put result-queue (assoc task :solution sol))
          (Thread/sleep 500)
          (recur id)))
      (debug "worker" id "got :die message"))))

(defn start-all-workers []
  (dotimes [id number-of-workers]
    (future (start-worker id))))

(defn kill-all-workers []
  (info "killing all workers")
  (dotimes [_ number-of-workers]
    (.put task-queue :die)))

(defn discard-tasks []
  (.clear task-queue))

(defn send-task [max-mass payload dv]
  (.put task-queue {:max-mass max-mass
                    :payload payload
                    :dv dv
                    :time (System/currentTimeMillis)}))

(defn get-result []
  (.take result-queue))

(defn any-solution [payload dv time-left]
  (info "find any initial solution")
  (let [initial-masses (range 200 701 50)]
    (doseq [mass initial-masses]
      (send-task mass payload dv))
    (loop [left (set initial-masses)]
      (cond (empty? left) (info "mass can't be more than 700 tons. no solution found :(")
            (neg? (time-left)) (info "time is up. no solution found :(")
            :else (let [{:keys [max-mass solution]} (get-result)]
                    (info "time left" (int (time-left)) "sec")
                    (if solution
                      solution
                      (do (info "no solution for mass " max-mass)
                          (recur (disj left max-mass)))))))))

(defn save-solution [sol payload]
  (info "\nfound solution with mass" (mass sol true payload) "and dv" (dv sol payload))
  (info "saved to solution.json\n")
  (spit "solution.json" (generate-string sol {:pretty true})))

(defn wait-for-improved-solution [current-mass time-left masses]
  (loop [left (set masses)]
    (info "time left" (int (time-left)) "sec")
    (if (pos? (time-left))
      (let [{:keys [solution max-mass] :as result} (get-result)]
        (cond (>= max-mass current-mass) (recur left)
              solution result
              :else (do (info "no solution for mass" max-mass)
                        (recur (disj left max-mass)))))
      (info "time is up"))))

(defn improve-solution [max-mass payload dv time-left]
  (discard-tasks)
  (let [masses (concat (range (- max-mass 10) max-mass)
                       (range (- max-mass 11) (- max-mass -50) -1))]
    (doseq [mass masses]
      (send-task mass payload dv))
    (when-let [improved (wait-for-improved-solution max-mass time-left masses)]
      (do (save-solution (:solution improved) payload)
          (recur (:max-mass improved) payload dv time-left)))))

;(find-stage 200 1.5 12000)

(defn run [payload dv timeout]
  (start-all-workers)
  (let [start (System/currentTimeMillis)
        end (+ start (* timeout 60 1000))
        time-left #(/ (- end (System/currentTimeMillis)) 1000.0)
        sol (any-solution payload dv time-left)]
    (discard-tasks)
    (when sol
      (save-solution sol payload)
      (improve-solution (int (mass sol true payload)) payload dv time-left)))
  (discard-tasks)
  (kill-all-workers)
  (shutdown-agents))


(defn -main [& [payload dv timeout]]
  (if dv
    (run (Double/parseDouble payload) (Double/parseDouble dv) (if timeout
                                                                (Double/parseDouble timeout)
                                                                2))
    (println "usage: lein run paylod dv [timeout]\n"
             "payload - mass need to deliver\n"
             "dv - delta V\n"
             "timeout - optional timeout for task in seconds. Default is 2.")))
