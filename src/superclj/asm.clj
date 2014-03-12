(ns superclj.asm)

(defn- assemble-one [ins]
  (cond
   (= ins '(clc)) [0x18]
   (= ins '(sec)) [0x38]
   (= ins '(xce)) [0xFB]
   :else (throw (IllegalArgumentException.
                 (str ins " is not a valid instruction sequence")))))

(assemble-one '(clc))

(defn asm [instructions]
  (vec (mapcat assemble-one instructions)))
