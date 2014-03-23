(ns superclj.asm)

(defn- double-to-byte [value]
  (let [high (bit-shift-right (bit-and 0xFF00 value) 8)
        low (bit-and 0xFF value)]
    [high low]))

(defn- flip-bytes [[high low]]
  [low high])

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
                 (= 'a const-type)  (flip-bytes (double-to-byte value))
                 :else (throw (IllegalArgumentException.
                               (str const-type " is not a valid constant type")))))
   :else (throw (IllegalArgumentException.
                 (str ins " is not a valid instruction sequence")))))

(defn- read-labels [instructions]
  (let [numbered-instructions (map-indexed (fn [i ins] [i ins]) instructions)
        labels (reduce (fn [labels [line-number ins]]
                         (if (= (first ins) 'label)
                           (assoc labels (second ins) line-number)
                           labels))
                       {} numbered-instructions)
        delabeled-instructions (map (fn [[op & args :as ins]]
                                      (if (= op 'label)
                                        (second args)
                                        ins))
                                    instructions)]
    [labels delabeled-instructions]))

(defn asm [instructions]
  (let [[labels delabeled-instructions] (read-labels instructions)]
    (vec (mapcat (fn [ins] (assemble-one ins labels))
                 delabeled-instructions))))
