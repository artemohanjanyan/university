(defn evaluate [expr vars]
  (expr (fn [f _ _] (f vars))))
(defn diff [expr var]
  (expr (fn [_ f _] (f var))))
(defn toString [expr]
  (expr (fn [_ _ s] s)))

(defn AbstractSupplier [evaluate diff string]
  (fn [f]
    (f evaluate diff string)))

(def zeroConst
  (AbstractSupplier
    (constantly 0)
    (constantly zeroConst)
    "0"))

(defn Constant [n]
  (AbstractSupplier
    (constantly n)
    (constantly zeroConst)
    (str n)))
(def oneConst (Constant 1))

(defn Variable [s]
  (AbstractSupplier
    (fn [vars] (get vars s))
    (fn [var] (if (= s var) oneConst zeroConst))
    s))

(defn fBy [f arg]
  (fn [expr]
    (f expr arg)))

(defn AbstractFunction [op diff string]
  (fn [& args]
    (fn [f]
      (f
        (fn [vars] (apply op (mapv (fBy evaluate vars) args)))
        (fn [var] (apply diff var args))
        (str "(" string " " (clojure.string/join " " (mapv toString args)) ")")))))

(def Add
  (AbstractFunction
    +
    (fn [var & args] (apply Add (mapv (fBy diff var) args)))
    "+"))

(def Subtract
  (AbstractFunction
    -
    (fn [var & args] (apply Subtract (mapv (fBy diff var) args)))
    "-"))

(def Negate
  (AbstractFunction
    -
    (fn [var arg] (Negate (diff arg var)))
    "negate"))

(def Multiply
  (AbstractFunction
    *
    (fn
      ([var arg] (diff arg var))
      ([var a & args]
        (Add
          (Multiply
            (diff a var)
            (apply Multiply args))
          (Multiply
            a
            (diff (apply Multiply args) var)))))
    ;		(fn [var & args]
    ;			(def argVec (vec args))
    ;			(apply Add
    ;				(map
    ;					(fn [n]
    ;						(apply
    ;							(apply partial Multiply (subvec argVec 0 n))
    ;							(diff (get argVec n) var)
    ;							(subvec argVec (+ n 1) (count argVec))))
    ;					(range 0 (count argVec)))))
    "*"))

(def Divide
  (AbstractFunction
    /
    (fn [var enum denom]
      (Divide
        (Subtract
          (Multiply (diff enum var) denom)
          (Multiply enum (diff denom var)))
        (Multiply denom denom)))
    "/"))

(declare Cos)

(def Sin
  (AbstractFunction
    (fn [x] (Math/sin x))
    (fn [var arg]
      (Multiply
        (Cos arg)
        (diff arg var)))
    "sin"))

(def Cos
  (AbstractFunction
    (fn [x] (Math/cos x))
    (fn [var arg]
      (Multiply
        (Negate (Sin arg))
        (diff arg var)))
    "cos"))

(defn parseObject [s]
  (def functions {"+" Add "-" Subtract "*" Multiply "/" Divide "negate" Negate "sin" Sin "cos" Cos})
  (declare parse)
  (defn parseF [name args]
    (apply (get functions (str name)) (map parse args)))
  (defn parse [x]
    (cond
      (symbol? x) (Variable (str x))
      (number? x) (Constant x)
      :else (parseF (first x) (rest x))))
  (parse (read-string s)))

;Don't know how to hide this from global scope
(deftype Function [operation priority argN])
(deftype State [operators output])

(defn parseObjectInfix [string]
  (def functions
    {"+" (Function. Add 2 2) "-" (Function. Subtract 2 2)
     "*" (Function. Multiply 1 2) "/" (Function. Divide 1 2)
     "negate" (Function. Negate 0 1)})

  (def bracket (Function. nil 100 nil))

  (defn applyOp [state]
    (def newFirst
      (apply (.operation (first (.operators state)))
        (reverse
          (take
            (.argN (first (.operators state)))
            (.output state)))))
    (def newRest
      (drop
        (.argN (first (.operators state)))
        (.output state)))
    (State.
      (rest (.operators state))
      (cons newFirst newRest)))

  (defn applyWhile [predicate state]
    (cond
      (empty? (.operators state)) state
      (predicate (first (.operators state))) (applyWhile predicate (applyOp state))
      :else state))

  (defn getPriority [f]
    (.priority f))

  (defn processToken [state token]
    (cond
      (= token "(") (State.
                      (cons bracket (.operators state))
                      (.output state))
      (= token ")") (let [newState (applyWhile
                                     (comp (partial not= (.priority bracket)) getPriority)
                                     state)]
                      (State.
                        (rest (.operators newState))
                        (.output newState)))
      (contains? functions token) (let [function (get functions token)
                                        newState (applyWhile
                                                   (comp (partial >= (.priority function)) getPriority)
                                                   state)]
                                    (State.
                                      (cons function (.operators newState))
                                      (.output newState)))
      :else (let [read (read-string token)]
              (State.
                (.operators state)
                (cons
                  (if (number? read)
                    (Constant read)
                    (Variable read))
                  (.output state))))))

  (def tokens (re-seq #"[0-9]+|\(|\)|[^0-9() ]+" string))
  (first
    (.output
      (applyWhile
        (constantly true)
        (reduce processToken (State. '() '()) tokens)))))

(println (toString (parseObjectInfix "(1 + x * negate 3) * (3 + 33)")))