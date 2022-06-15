(ns tuh8888.clojure-utils)

(defn vals-as-keys [k m] (zipmap (map k m) m))

#_(defmacro cond-pred->
    "Like cond-> but also threads initial expr through tests."
    {:added "1.5"}
    [expr & clauses]
    (assert (even? (count clauses)))
    (let [g     (gensym)
          steps (map (fn [[test step]]
                       `(if (-> ~g
                                ~test)
                          (-> ~g
                              ~step)
                          ~g))
                     (partition 2 clauses))]
      `(let [~g ~expr
             ~@(interleave (repeat g) (butlast steps))]
         ~(if (empty? steps) g (last steps)))))

(defmacro cond-pred->
  ([x pred body]
   `(cond-> ~x
      (~pred ~x) ~body))
  ([x body] `(cond-pred-> ~x identity ~body)))

(defmacro cond-pred->>
  ([x pred body] `(cond->> ~x (~pred ~x) ~body))
  ([x body] `(cond-pred->> ~x identity ~body)))

(defmacro or->
  ([x body]
   `(or (-> ~x
            ~body)
        ~x)))

(defn cljs-update-vals
  [m f]
  (->> m
       (map (juxt key (comp f val)))
       (into {})))
