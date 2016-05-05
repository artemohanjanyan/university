(defn fib [n]
  (letfn
    [(fib' [n a b]
       (if (= 0 n)
         a
         (recur (- n 1) b (+' a b))))]
    (fib' n 1 1)))

(defn foldLeft [zero f xs]
  (if (empty? xs)
    zero
    (recur (f zero (first xs)) f (rest xs))))

(defn foldRight [zero f xs]
  (if (empty? xs)
    zero
    (f (first xs) (foldRight zero f (rest xs)))))

(println (fib 10))
(println (foldLeft 0 + '(1 2 3 4 5)))
(println (foldRight 0 + '(1 2 3 4)))

(defn swapArgs [f]
  (fn [a b & rest]
    (apply f b a rest)))



(defn parseObjectInfix [string]
  (def tokens (re-seq #"[0-9]+|\(|\)|[^0-9() ]+" string))
  ;	(def functions { 2 {"+" Add "-" Subtract}
  ;									 1 {"*" Multiply "/" Divide}
  ;									 0 {"-" Negate}})
  (def functions
    {"+" {:op Add :pr 2 :argn 2} "-" {:op Subtract :pr 2 :argn 2}
     "*" {:op Multiply :pr 1 :argn 2} "/" {:op Divide :pr 1 :argn 2}
     "negate" {:op Negate :pr 0 :argn 1}})
  (defn applyOp [state]
    (def newFirst
      (reverse
        (take
          (:argn (get functions (:operators state)))
          (:output state))))
    (def newRest
      (drop
        (:argn (get functions (:operators state)))
        (:output state)))
    {:operators (rest (:operators state))
     :output (cons newFirst newRest)})
  (reduce
    (fn [state token]
      (cond
        (contains? functions token) ))
    {:operators '() :output '()} tokens))