(ns kenken-puzzle-solver.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic #_#_ :as logic :refer :all])
  (:require [clojure.core.logic.fd :as fd])
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def p1 {:size 3
         :constraints [{:f =
                        :res 3
                        :cells [0]}
                       {:f -
                        :res 1
                        :cells [1 2]}
                       {:f *
                        :res 6
                        :cells [6 7]}]
         })

; example hardcoded solution
(run 10 [q]
  (fresh [a0 a1 a2
          a3 a4 a5
          a6 a7 a8]
    (== q [a0 a1 a2 a3 a4 a5 a6 a7 a8])

    (fd/in a0 a1 a2 a3 a4 a5 a6 a7 a8 (fd/interval 1 3))

    ; rows
    (fd/distinct [a0 a1 a2])
    (fd/distinct [a3 a4 a5])
    (fd/distinct [a6 a7 a8])
    ; collumns
    (fd/distinct [a0 a3 a6])
    (fd/distinct [a1 a4 a7])
    (fd/distinct [a2 a5 a8])

    (fd/eq (= 3 a0))
    (conde
     [(fd/eq (= 1 (- a1 a2)))]
     [(fd/eq (= 1 (- a2 a1)))])
    (fd/eq (= 6 (* a6 a7)))
    ))


(defn rows-helper
  [v dim res]
  (cond
    (= 0 (count v)) res
    :else (rows-helper (subvec v dim) dim (conj res (vec (take dim v))))))
; v = [1 2 3 4 5 6 7 8 9]
; --> [[1 2 3] [4 5 6] [7 8 9]]
(rows-helper [1 2 3 4 5 6 7 8 9] 3 [])

(defn rows
  [v dim]
  (rows-helper v dim []))
(rows [1 2 3 4 5 6 7 8 9] 3)


(defn cols
  [rows]
  (apply mapv vector rows))
(cols [[1 2 3] [4 5 6] [7 8 9]])

;;;;;;;;;;;;

(defn logic-distinct
  [vars]
  `(fd/distinct [~@vars]))
(logic-distinct ['a 'b 'c])

;;;;;;;;;;;;

(defn logic-minus-helper
  [targ cells]
  (let [feq (fn [cells] `(fd/eq (= ~targ (- ~@cells))))
        perms (combo/permutations cells)
        clauses (map vector (map feq perms))]
    `(conde ~@clauses)))

(defn logic-divide-helper
  [targ cells]
  (let [feq (fn [cells] `(fd/eq (= ~targ (/ ~@cells))))
        perms (combo/permutations cells)
        clauses (map vector (map feq perms))]
    `(conde ~@clauses)))

(defn logic-conde-helper
  [targ cells f]
  (let [feq (fn [cells] `(fd/eq (= ~targ (~f ~@cells))))
        perms (combo/permutations cells)
        clauses (map vector (map feq perms))]
    `(conde ~@clauses)))

(defn logic-constraint
  [c vars]
  (let [f (c :f)
        targ (c :res)
        cell-indices (c :cells)
        cells (mapv vars cell-indices)]
    (cond
      (= f =) `(fd/eq (= ~targ ~@cells))
      (= f +) `(fd/eq (= ~targ (+ ~@cells)))
      (= f *) `(fd/eq (= ~targ (* ~@cells)))
      (= f -) (logic-minus-helper targ cells)
      (= f /) (logic-divide-helper targ cells)
      ; (= f -) (logic-conde-helper targ cells -)
      ; (= f /) (logic-conde-helper targ cells /)
      )))

;;;;;;;;;;;;

(defn solve-puzzle
  [p]
  (let [q (gensym 'q)
        size (p :size)
        syms (vec (map (fn [x] (gensym 'a)) (range (* size size))))
        rowsss (rows syms size)
        colsss (cols rowsss)
        distinct-rows (map logic-distinct rowsss)
        distinct-cols (map logic-distinct colsss)
        constraints (p :constraints)
        logic-clauses (map #(logic-constraint % syms) constraints)]
    `(run 10 [~q]
       (fresh [~@syms]
         (== ~q [~@syms])
         (fd/in ~@syms (fd/interval 1 ~size))

         ~@distinct-rows
         ~@distinct-cols

         ~@logic-clauses
         )
       )
    ))

(def p1-solution (solve-puzzle p1))
(def p1-sol-string (str p1-solution))
(def p1-sol-to-eval (clojure.string/replace p1-sol-string "clojure.core/" ""))
(eval (read-string p1-sol-to-eval))

(defn eval-solve-puzzle
  [p]
  (let [sol (solve-puzzle p)
        sol-string (str sol)
        to-eval (clojure.string/replace sol-string "clojure.core/" "")]
    (eval (read-string to-eval))))

; 6x6 puzzle
(def p2 {:size 6
         :constraints [{:f +
                        :res 6
                        :cells [0 6]}
                       {:f *
                        :res 40
                        :cells [1 7 13]}
                       {:f -
                        :res 5
                        :cells [2 3]}
                       {:f *
                        :res 36
                        :cells [4 5 10]}
                       {:f /
                        :res 3
                        :cells [12 18]}
                       {:f -
                        :res 1
                        :cells [8 14]}
                       {:f *
                        :res 12
                        :cells [9 15 16]}
                       {:f /
                        :res 3
                        :cells [11 17]}
                       {:f *
                        :res 72
                        :cells [19 24 25]}
                       {:f -
                        :res 2
                        :cells [30 31]}
                       {:f +
                        :res 3
                        :cells [20 21]}
                       {:f +
                        :res 7
                        :cells [26 32]}
                       {:f -
                        :res 1
                        :cells [22 23]}
                       {:f *
                        :res 10
                        :cells [27 28]}
                       {:f /
                        :res 3
                        :cells [33 34]}
                       {:f -
                        :res 4
                        :cells [29 35]}
                       ]})


(defn pretty-solution
  [p]
  (let [size (p :size)]
    (rows (first (eval-solve-puzzle p)) size)))

(pretty-solution p2)

