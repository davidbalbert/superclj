(ns superclj.65816-test
  (:require [clojure.test :refer :all]
            [superclj.65816 :refer :all]
            [superclj.asm :as asm]
            [superclj.memory :as mem]))

(defn run-asm [cpu instructions]
  (run cpu (mem/memory-from-vec (asm/asm instructions))))

(defn with-carry-set [cpu]
  (assoc cpu :status (bit-set (status cpu) 0)))

(deftest clc-should-clear-carry
  (let [start-cpu (with-carry-set (new-cpu))
        end-cpu (first (run-asm start-cpu '((clc) (stp))))]
    (is (= 0 (carry end-cpu)))))

(deftest sec-should-set-carry
  (let [cpu (first (run-asm (new-cpu) '((sec) (stp))))]
    (is (= 1 (carry cpu)))))

(deftest xce-should-exchange-carry-and-emulation-bits
  (let [cpu (first (run-asm (new-cpu) '((xce) (stp))))]
    (is (= 0 (cpu :emulation-mode)))
    (is (= 1 (carry cpu)))))

(deftest txy-should-transfer-x-to-y
  (let [start-cpu (assoc (new-cpu) :x 0x10)
        end-cpu (first (run-asm start-cpu '((txy) (stp))))]
    (is (= 0x10 (end-cpu :y)))))

(deftest tax-should-transfer-a-to-x
  (let [start-cpu (assoc (new-cpu) :a 0x10)
        end-cpu (first (run-asm start-cpu '((tax) (stp))))]
    (is (= 0x10 (end-cpu :x)))))

(deftest tay-should-transfer-a-to-y
  (let [start-cpu (assoc (new-cpu) :a 0x10)
        end-cpu (first (run-asm start-cpu '((tay) (stp))))]
    (is (= 0x10 (end-cpu :y)))))

(deftest txa-should-transfer-x-to-a
  (let [start-cpu (assoc (new-cpu) :x 0x10)
        end-cpu (first (run-asm start-cpu '((txa) (stp))))]
    (is (= 0x10 (end-cpu :a)))))

(deftest tya-should-transfer-y-to-a
  (let [start-cpu (assoc (new-cpu) :y 0x10)
        end-cpu (first (run-asm start-cpu '((tya) (stp))))]
    (is (= 0x10 (end-cpu :a)))))

(deftest stp-should-stop-the-processor
  (let [start-cpu (new-cpu)
        end-cpu (first (run-asm start-cpu '((stp) (sec))))]
    (is (= 0 (carry end-cpu)))))

(run-tests)
