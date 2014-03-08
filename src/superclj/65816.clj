(ns superclj.65816)

(defn new-cpu []
  {:emulation-mode 1
   :registers {
               :a 0x00
               :x 0x00
               :y 0x00
               :direct-page 0x0000
               :sp 0
               :pc 0
               :program-bank 0x00
               :data-bank 0x00
               :status 0}})
