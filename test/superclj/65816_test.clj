(ns superclj.65816-test
  (:require [clojure.test :refer :all]
            [superclj.65816 :refer :all]

(def c (new-cpu))
            [superclj.asm :as asm]
            [superclj.memory :as mem]))

(defn run-asm [cpu instructions]
  (run cpu (mem/memory-from-vec (asm/asm instructions))))

(defn with-carry-set [cpu]
  (assoc cpu :status (bit-set (status cpu) 0)))

(defn with-carry-cleared [cpu]
  (assoc cpu :status (bit-clear (status cpu) 0)))

(deftest clc-should-clear-carry
  (let [start-cpu (with-carry-set c)
        end-cpu (first (run-asm start-cpu '((clc))))]
    (is (= 0 (carry end-cpu)))))

(deftest sec-should-set-carry
  (let [start-cpu (with-carry-cleared c)
        end-cpu (first (run-asm start-cpu '((sec))))]
    (is (= 1 (carry end-cpu)))))

(deftest xce-should-exchange-carry-and-emulation-bits
  (let [start-cpu (with-carry-cleared c)
        end-cpu (first (run-asm start-cpu '((xce))))]
    (is (= 0 (:emulation-mode end-cpu)))
    (is (= 1 (carry end-cpu)))))

(deftest txy-should-transfer-x-to-y
  (let [start-cpu (assoc c :x 0x10)
        end-cpu (first (run-asm start-cpu '((txy))))]
    (is (= 0x10 (:y end-cpu)))))

(deftest tax-should-transfer-a-to-x
  (let [start-cpu (assoc (new-cpu) :a 0x10)
        end-cpu (first (run-asm start-cpu '((tax))))]
    (is (= 0x10 (:x end-cpu)))))

(deftest tay-should-transfer-a-to-y
  (let [start-cpu (assoc (new-cpu) :a 0x10)
        end-cpu (first (run-asm start-cpu '((tay))))]
    (is (= 0x10 (:y end-cpu)))))

(deftest txa-should-transfer-x-to-a
  (let [start-cpu (assoc (new-cpu) :x 0x10)
        end-cpu (first (run-asm start-cpu '((txa))))]
    (is (= 0x10 (:a end-cpu)))))

(deftest tya-should-transfer-y-to-a
  (let [start-cpu (assoc (new-cpu) :y 0x10)
        end-cpu (first (run-asm start-cpu '((tya))))]
    (is (= 0x10 (:a end-cpu)))))

(run-tests)
