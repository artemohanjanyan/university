(def constant constantly)
(defn variable [s] (fn [vars] (get vars s)))

(defn abstractFunction [f]
  (fn [& args]
    (fn [vars]
      (apply f (mapv (fn [expr] (expr vars)) args)))))
(def add (abstractFunction +))
(def subtract (abstractFunction -))
(def multiply (abstractFunction *))
(def divide (abstractFunction (fn [^double a ^double b] (/ a b))))
(def negate subtract)
(def sin (abstractFunction (fn [x] (Math/sin x))))
(def cos (abstractFunction (fn [x] (Math/cos x))))

(defn parseFunction [s]
  (def functions {"+" add "-" subtract "*" multiply "/" divide "negate" negate "sin" sin "cos" cos})
  (declare parse)
  (defn parseF [name args]
    (apply (get functions (str name)) (map parse args)))
  (defn parse [x]
    (cond
      (symbol? x) (variable (str x))
      (number? x) (constant x)
      :else (parseF (first x) (rest x))))
  (parse (read-string s)))

;(defn parseObjectInfix [string]
;	(def tokens (re-seq #"[0-9]+|\(|\)|[^0-9() ]+" string))
;	(def functions
;		{"+" {:op Add :pr 2 :argn 2} "-" {:op Subtract :pr 2 :argn 2}
;		 "*" {:op Multiply :pr 1 :argn 2} "/" {:op Divide :pr 1 :argn 2}
;		 "negate" {:op Negate :pr 0 :argn 1}})
;	(defn applyOp [state]
;		(def newFirst
;			(reverse
;				(take
;					(:argn (get functions (:operators state)))
;					(:output state))))
;		(def newRest
;			(drop
;				(:argn (get functions (:operators state)))
;				(:output state)))
;		{:operators (rest (:operators state))
;		 :output (cons newFirst newRest)})
;	(defn applyWhile )
;	(reduce
;		(fn [state token]
;			(cond
;				(contains? functions token) ))
;		{:operators '() :output '()} tokens))