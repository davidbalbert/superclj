(ns superclj.asm)

(defn- assemble-one [ins]
  (cond
   (= ins '(clc)) [0x18]
   (= ins '(sec)) [0x38]
   (= ins '(xce)) [0xFB]
   (= ins '(txy)) [0x9B]
   (= ins '(tax)) [0xAA]
   (= ins '(tay)) [0xA8]
   :else (throw (IllegalArgumentException.
                 (str ins " is not a valid instruction sequence")))))

(defn asm [instructions]
  (vec (mapcat assemble-one instructions)))
