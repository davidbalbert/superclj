(ns superclj.65816)

(defn new-cpu []
  {:a 0
   :x 0
   :y 0
   :direct-page 0
   :sp 0
   :pc 0
   :program-bank 0
   :data-bank 0
   :status 0})
