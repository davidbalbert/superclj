(ns superclj.asm)

(defn- double-to-byte [value]
  (let [high (bit-shift-right (bit-and 0xFF00 value) 8)
        low (bit-and 0xFF value)]
    [high low]))

(defn- swap-bytes [[high low]]
  [low high])

(defn- num->byte-vector [number]
  (loop [n number result []]
    (if (zero? n)
      (reverse result)
      (recur (bit-shift-right n 8)
             (conj result (bit-and 0xFF n))))))

(defn- assemble-one [[op & args :as ins] labels]
  (cond
   (= ins '(clc)) [0x18]
   (= ins '(sec)) [0x38]
   (= ins '(xce)) [0xFB]
   (= ins '(txy)) [0x9B]
   (= ins '(tax)) [0xAA]
   (= ins '(tay)) [0xA8]
   (= ins '(txa)) [0x8A]
   (= ins '(tya)) [0x98]
   (= op 'ds) (let [bytes (first args)]
                (vec (repeat bytes 0)))
   (= op 'dc) (let [[const-type value] args]
                (cond
                 (= 'i1 const-type) [(bit-and 0xFF value)]
                 (= 'a const-type)  (swap-bytes (double-to-byte value))
                 (= 'c const-type)  (map byte (vec value))
                 (= 'h const-type)  (num->byte-vector value)
                 :else (throw (IllegalArgumentException.
                               (str const-type " is not a valid constant type")))))
   :else (throw (IllegalArgumentException.
                 (str ins " is not a valid instruction sequence")))))

(defn read-labels [instructions]
  (reduce (fn [labels [i [op name]]]
            (if (= 'label op)
              (assoc labels name i)
              labels))
          {}
          (map-indexed vector instructions)))

(defn remove-labels [instructions]
  (vec (map
        (fn [[op :as ins]]
          (if (= 'label op)
            (nth ins 2)
            ins))
        instructions)))

(defn asm [instructions]
  (let [labels (read-labels instructions)
        delabeled-instructions (remove-labels instructions)]
    (vec (mapcat (fn [ins] (assemble-one ins labels))
                 delabeled-instructions))))
