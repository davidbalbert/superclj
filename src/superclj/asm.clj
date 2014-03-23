(ns superclj.asm)

(defn- assemble-one [ins labels]
  (cond
   (= ins '(clc)) [0x18]
   (= ins '(sec)) [0x38]
   (= ins '(xce)) [0xFB]
   (= ins '(txy)) [0x9B]
   (= ins '(tax)) [0xAA]
   (= ins '(tay)) [0xA8]
   (= ins '(txa)) [0x8A]
   (= ins '(tya)) [0x98]
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
