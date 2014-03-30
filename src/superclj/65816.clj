(ns superclj.65816
  (:require [superclj.memory :as mem]))

;; TODO: Split :status into a Map
(defn new-cpu []
  {:running true
   :emulation-mode 0x1
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

(defn txy [cpu mem]
  (let [new-cpu (assoc cpu :y (cpu :x))]
    [new-cpu mem]))

; TODO: the index registers and accumulator can be different
; widths. Figure out how that should work.
(defn tax [cpu mem]
  (let [new-cpu (assoc cpu :x (cpu :a))]
    [new-cpu mem]))

; TODO: the index registers and accumulator can be different
; widths. Figure out how that should work.
(defn tay [cpu mem]
  (let [new-cpu (assoc cpu :y (cpu :a))]
    [new-cpu mem]))

; TODO: the index registers and accumulator can be different
; widths. Figure out how that should work.
(defn txa [cpu mem]
  (let [new-cpu (assoc cpu :a (cpu :x))]
    [new-cpu mem]))

; TODO: the index registers and accumulator can be different
; widths. Figure out how that should work.
(defn tya [cpu mem]
  (let [new-cpu (assoc cpu :a (cpu :y))]
    [new-cpu mem]))

(defn stp [cpu mem]
  [(assoc cpu :running false) mem])

(def opcodes
  {0x18 clc
   0x38 sec
   0xFB xce
   0x9B txy
   0xAA tax
   0xA8 tay
   0x8A txa
   0x98 tya
   0xDB stp})

(defn step [cpu mem]
  (let [opcode (mem/load-byte mem (pc cpu))]
    ((opcodes opcode) (increment-pc cpu) mem)))

(defn run [cpu mem]
  (if (cpu :running)
    (let [[next-cpu next-mem] (step cpu mem)]
      (recur next-cpu next-mem))
    [cpu mem]))
