(defproject kenken-puzzle-solver "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.logic "1.0.1"]
                 [org.clojure/math.combinatorics "0.2.0"]]
  :main ^:skip-aot kenken-puzzle-solver.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
