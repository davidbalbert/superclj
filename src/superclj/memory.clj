(ns superclj.memory)

(defn kilobytes [n]
  (int (* n (Math/pow 2 10))))

(defn allocate-memory [size]
  (vec (repeat size 0)))

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

(defn store-byte [memory address byte]
  (throw-unless-byte byte)
  (assoc memory address byte))

(defn load-byte [memory address]
  (memory address))

(defn store-double [memory address double-byte]
  (throw-unless-double double-byte)
  (let [high (bit-shift-right (bit-and 0xFF00 double-byte) 8)
        low (bit-and 0x00FF double-byte)]
    (-> memory
        (assoc ,,, address low)
        (assoc ,,, (inc address) high))))

(defn load-double [memory address]
  (let [low (memory address)
        high (memory (inc address))]
    (+ (bit-shift-left high 8) low)))
