(ns superclj.util)

(defn throw-arg-exception [& strings]
  (throw (IllegalArgumentException.
          (apply str strings))))