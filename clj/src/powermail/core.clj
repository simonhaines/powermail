(ns powermail.core
  (:refer-clojure)
  (:require [clj-time.local :as local]))

; User preferences (able to be re-bound)
(def ^:dynamic *time-ref* (local/local-now))
(def ^:dynamic *spec-ref* 'MDY)


(defn -main [& args]
  (println "Hello, world!"))
