(ns superclj.memory)

(defn kilobytes [n]
  (int (* n (Math/pow 2 10))))

(defn- throw-unless-range [num min max]
  (cond
   (< num min) (throw (IllegalArgumentException.
                       (format "0x%X is less than 0x%X" num min)))
   (> num max) (throw (IllegalArgumentException.
                       (format "0x%X is greater than 0x%X" num max)))
   :else num))

(defn- throw-unless-byte [num]
  (throw-unless-range num 0x0 0xFF))

(defn- throw-unless-double [num]
  (throw-unless-range num 0x0 0xFFFF))

(defprotocol IMemory
  (store-byte [this address value])
  (load-byte [this address])
  (store-double [this address value])
  (load-double [this address]))

(deftype VectorMemory [v]
  IMemory
  (store-byte [this address value]
    (throw-unless-byte value)
    (VectorMemory. (assoc v address value)))

  (load-byte [this address]
    (v address))

  (store-double [this address value]
    (throw-unless-double value)
    (let [high (bit-shift-right (bit-and 0xFF00 value) 8)
          low (bit-and 0x00FF value)]
      (-> v
          (assoc ,,, address low)
          (assoc ,,, (inc address) high)
          VectorMemory.)))

  (load-double [this address]
    (let [low (v address)
          high (v (inc address))]
      (+ (bit-shift-left high 8) low)))

  Object
  (toString [this]
    (str v))

  clojure.lang.Counted
  (count [this]
    (count v)))

(defn allocate-memory [size]
  (->VectorMemory (vec (repeat size 0))))

(def memory-from-vec ->VectorMemory)
