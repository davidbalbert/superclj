(ns superclj.asm
  (require [superclj.util :as util]))

(defn- double->bytes [value]
  (let [high (bit-shift-right (bit-and 0xFF00 value) 8)
        low (bit-and 0xFF value)]
    [high low]))

(defn- swap-bytes [[high low]]
  [low high])

(def double->address (comp swap-bytes double->bytes))

(defn- num->byte-vector [number]
  (loop [n number result []]
    (if (zero? n)
      (reverse result)
      (recur (bit-shift-right n 8)
             (conj result (bit-and 0xFF n))))))

(defn- generate-intermediate-code [[op & args :as ins]]
  (cond
   (= ins '(clc)) [0x18]
   (= ins '(sec)) [0x38]
   (= ins '(xce)) [0xFB]
   (= ins '(txy)) [0x9B]
   (= ins '(tax)) [0xAA]
   (= ins '(tay)) [0xA8]
   (= ins '(txa)) [0x8A]
   (= ins '(tya)) [0x98]
   (= ins '(stp)) [0xDB]
   (= op 'lda)    [0xAD (if (symbol? (first args))
                          (first args)
                          (double->address (first args)))]
   (= op 'ds) (let [bytes (first args)]
                (vec (repeat bytes 0)))
   (= op 'dc) (let [[const-type value] args]
                (cond
                 (= 'i1 const-type) [(bit-and 0xFF value)]
                 (= 'a const-type)  (double->address value)
                 (= 'c const-type)  (map byte (vec value))
                 (= 'h const-type)  (num->byte-vector value)
                 :else (util/throw-arg-exception const-type " is not a valid constant type")))
   :else (util/throw-arg-exception ins " is not a valid instruction sequence")))

(defn mapify-instruction [ins]
  {:instruction ins})

(defn label-first [label instructions]
  (let [labeled-first-instruction (assoc (first instructions) :label label)]
    (cons labeled-first-instruction (rest instructions))))

(defn inline-label [[label & instructions]]
   (let [instructions-as-maps (map mapify-instruction instructions)]
     (if (= label '_)
       instructions-as-maps
       (label-first label instructions-as-maps))))

;; Labels represent addresses, which are assumed to be two bytes.
;; This may be different in long addressing modes, but I'm not sure.
(defn count-intermediate-code [v]
  (reduce + (map (fn [byte-or-symbol]
                   (if (symbol? byte-or-symbol)
                     2
                     1))
                 v)))

(defn assemble-one [instruction]
  (let [intermediate-code (generate-intermediate-code (instruction :instruction))
        size (count-intermediate-code intermediate-code)]
    (-> instruction
        (assoc :intermediate-code intermediate-code)
        (assoc :size size))))

(defn calculate-offsets [instructions]
  (let [sizes (map :size instructions)
        totals (reductions + sizes)
        offsets (cons 0 (butlast totals))]
    (map (fn [ins offset]
           (assoc ins :offset offset))
         instructions
         offsets)))

(defn make-labels [instructions]
  (reduce (fn [labels ins]
            (if (contains? labels (ins :label))
              (util/throw-arg-exception "Label `"(ins :label) "' has already been used")
              (assoc labels (ins :label) (double->address (ins :offset)))))
          {}
          (filter :label instructions)))

(defn replace-label [intermediate-code labels]
  (flatten (map (fn [byte-or-symbol]
                  (if (symbol? byte-or-symbol)
                    (if (contains? labels byte-or-symbol)
                      (labels byte-or-symbol)
                      (util/throw-arg-exception "Unknown label `" byte-or-symbol "'"))
                    byte-or-symbol))
                intermediate-code)))

(defn replace-labels [instructions labels]
  (let [machine-code (map (fn [ins]
                            (replace-label (ins :intermediate-code) labels))
                          instructions)]
    (map (fn [ins bytes]
           (assoc ins :machine-code bytes))
         instructions
         machine-code)))

;; asm syntax: (& [label & instructions])
;; example: a program that loads a number
;; at label 'num into the accumulator and
;; then stops the clock:
;;
;; '((_    (lda num)
;;         (stp))
;;   (num  (dc i1 42))
;;
;; All fragments must have a label. The '_
;; can be be used to throw away the label.
(defn asm [labeled-fragments]
  (let [labeled-instructions (mapcat inline-label labeled-fragments)
        intermediate-instructions (map assemble-one labeled-instructions)
        offset-instructions (calculate-offsets intermediate-instructions)
        labels (make-labels offset-instructions)
        assembeled-instructions (replace-labels offset-instructions labels)]
    (vec (mapcat :machine-code assembeled-instructions))))
