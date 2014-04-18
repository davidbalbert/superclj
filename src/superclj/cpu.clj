(ns superclj.cpu
  (require [superclj.util :as util]
           [superclj.memory :as mem]))

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
   :status 2r00000000})

(defn- pc [cpu]
  (cpu :pc))

(defn status [cpu]
  (cpu :status))

(defn carry [cpu]
  (bit-and (status cpu) 0x1))

(defn a [cpu]
  (cpu :a))

(defn- increment-pc [cpu]
  (let [new-pc (inc (pc cpu))]
    (assoc cpu :pc new-pc)))

(defn- assign-bit [byte position value]
  (if (zero? value)
    (bit-clear byte position)
    (bit-set byte position)))

(defn memory-select [cpu]
  (bit-shift-right (bit-and (status cpu) 2r00100000) 5))

(defn index-select [cpu]
  (bit-shift-right (bit-and (status cpu) 2r00010000) 4))

(defn zero-bit [cpu]
  (bit-shift-right (bit-and (status cpu) 2r00000010) 1))

(defn negative-bit [cpu]
  (bit-shift-right (bit-and (status cpu) 2r10000000) 7))

(defn emulation-mode? [cpu]
  (= 1 (cpu :emulation-mode)))

(defn clc [cpu mem]
  (let [new-status (bit-and (status cpu) 2r11111110)
        new-cpu (assoc cpu :status new-status)]
    [new-cpu mem]))

(defn sec [cpu mem]
  (let [new-status (bit-or (status cpu) 0x1)
        new-cpu (assoc cpu :status new-status)]
    [new-cpu mem]))

(defn xce [cpu mem]
  (let [m-x-value (if (zero? (carry cpu)) 1 0)
        new-status (-> (status cpu)
                       (assign-bit 0 (cpu :emulation-mode))
                       (assign-bit 4 m-x-value)
                       (assign-bit 5 m-x-value))
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

(defn twos-complement-negative? [number-of-bytes value]
  (cond
   (= 1 number-of-bytes) (not (zero? (bit-and value 0x80)))
   (= 2 number-of-bytes) (not (zero? (bit-and value 0x8000)))
   :else (util/throw-arg-exception number-of-bytes " is not a valid number of bytes")))

(def status-name->position
  {:negative 7
   :overflow 6
   :memory-select 5
   :index-select 4
   :decimal-mode 3
   :irq-disable 2
   :zero 1
   :carry 0})

(defn set-status-bit-to-value [old-status status-name value]
  (cond
   (zero? value) (bit-clear old-status (status-name->position status-name))
   (= 1 value) (bit-set old-status (status-name->position status-name))
   :else (util/throw-arg-exception value " must be 1 or 0")))

(twos-complement-negative? 1 2r10000000)

(defn lda [cpu mem]
  (let [number-of-bytes (if (or (emulation-mode? cpu)
                                (= 1 (memory-select cpu)))
                          1 2)
        address (mem/load-double mem (pc cpu))
        value (if (= 2 number-of-bytes)
                (mem/load-double mem address)
                (mem/load-byte mem address))
        zero (if (zero? value) 1 0)
        negative (if (twos-complement-negative? number-of-bytes value) 1 0)
        new-pc (+ 2 (pc cpu))
        new-status (-> (status cpu)
                       (set-status-bit-to-value :zero zero)
                       (set-status-bit-to-value :negative negative))
        new-cpu (-> cpu
                    (assoc :a value)
                    (assoc :pc new-pc)
                    (assoc :status new-status))]
    [new-cpu mem]))

(def opcodes
  {0x18 clc
   0x38 sec
   0xFB xce
   0x9B txy
   0xAA tax
   0xA8 tay
   0x8A txa
   0x98 tya
   0xDB stp
   0xAD lda})

(defn step [cpu mem]
  (let [opcode (mem/load-byte mem (pc cpu))]
    ((opcodes opcode) (increment-pc cpu) mem)))

(defn run [cpu mem]
  (if (cpu :running)
    (let [[next-cpu next-mem] (step cpu mem)]
      (recur next-cpu next-mem))
    [cpu mem]))
