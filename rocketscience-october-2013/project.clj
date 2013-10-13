(defproject rocketscience "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [cheshire "5.2.0"]
                 [org.clojure/tools.logging "0.2.6"]
                 [log4j "1.2.16"]]

  :main rocketscience.core
  :resource-paths ["resources"]
  :aot :all)
