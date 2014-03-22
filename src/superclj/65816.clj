(ns superclj.65816
  (:require [superclj.memory :as mem]
            [superclj.asm :as asm]))

(def opcodes (atom {}))

(defn new-cpu []
  {:emulation-mode 0x1
   :a 0x00
   :x 0x00
   :y 0x00
   :direct-page 0x0000
   :sp 0
   :pc 0
   :program-bank 0x00
   :data-bank 0x00
   :status 2r00110000})

(defn- pc [cpu]
  (cpu :pc))

(defn status [cpu]
  (cpu :status))

(defn carry [cpu]
  (bit-and (status cpu) 0x1))

(defn- increment-pc [cpu]
  (let [new-pc (inc (pc cpu))]
    (assoc cpu :pc new-pc)))

(defn- assign-bit [byte position value]
  (if (zero? value)
    (bit-clear byte position)
    (bit-set byte position)))

(defn step [cpu mem]
  (let [opcode (mem/load-byte mem (pc cpu))]
    ((@opcodes opcode) (increment-pc cpu) mem)))

(defn run [cpu mem]
  (if (< (pc cpu) (count mem))
    (let [[next-cpu next-mem] (step cpu mem)]
      (recur next-cpu next-mem))
    [cpu mem]))

(defn clc [cpu mem]
  (let [new-status (bit-and (status cpu) 2r11111110)
        new-cpu (assoc cpu :status new-status)]
    [new-cpu mem]))

(defn sec [cpu mem]
  (let [new-status (bit-or (status cpu) 0x1)
        new-cpu (assoc cpu :status new-status)]
    [new-cpu mem]))

(defn xce [cpu mem]
  (let [new-status (assign-bit (status cpu) 0 (cpu :emulation-mode))
        new-emulation-mode (carry cpu)
        new-cpu (-> cpu
                    (assoc :emulation-mode new-emulation-mode)
                    (assoc :status new-status))]
    [new-cpu mem]))

(swap! opcodes (fn [_]
                 {
                   0x18 clc
                   0x38 sec
                   0xFB xce}))
