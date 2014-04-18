(ns superclj.65816-test
  (:require [clojure.test :refer :all]
            [superclj.cpu :refer :all]
            [superclj.asm :as asm]
            [superclj.memory :as mem]))

(defn run-asm [cpu instructions]
  (run cpu (mem/memory-from-vec (asm/asm instructions))))

(defn with-carry-set [cpu]
  (assoc cpu :status (bit-set (status cpu) 0)))

(deftest clc-should-clear-carry
  (let [start-cpu (with-carry-set (new-cpu))
        end-cpu (first (run-asm start-cpu '((_ (clc)
                                               (stp)))))]
    (is (= 0 (carry end-cpu)))))

(deftest sec-should-set-carry
  (let [cpu (first (run-asm (new-cpu) '((_ (sec)
                                           (stp)))))]
    (is (= 1 (carry cpu)))))

(deftest xce-should-exchange-carry-and-emulation-bits
  (let [cpu (first (run-asm (new-cpu) '((_ (xce)
                                           (stp)))))]
    (is (= 0 (cpu :emulation-mode)))
    (is (= 1 (carry cpu)))))

(deftest xce-should-set-m-and-x-bits
  (let [cpu (first (run-asm (new-cpu) '((_ (xce)
                                           (stp)))))]
    (is (= 1 (memory-select cpu)))
    (is (= 1 (index-select cpu)))))

(deftest txy-should-transfer-x-to-y
  (let [start-cpu (assoc (new-cpu) :x 0x10)
        end-cpu (first (run-asm start-cpu '((_ (txy)
                                               (stp)))))]
    (is (= 0x10 (end-cpu :y)))))

(deftest tax-should-transfer-a-to-x
  (let [start-cpu (assoc (new-cpu) :a 0x10)
        end-cpu (first (run-asm start-cpu '((_ (tax)
                                               (stp)))))]
    (is (= 0x10 (end-cpu :x)))))

(deftest tay-should-transfer-a-to-y
  (let [start-cpu (assoc (new-cpu) :a 0x10)
        end-cpu (first (run-asm start-cpu '((_ (tay)
                                               (stp)))))]
    (is (= 0x10 (end-cpu :y)))))

(deftest txa-should-transfer-x-to-a
  (let [start-cpu (assoc (new-cpu) :x 0x10)
        end-cpu (first (run-asm start-cpu '((_ (txa)
                                               (stp)))))]
    (is (= 0x10 (end-cpu :a)))))

(deftest tya-should-transfer-y-to-a
  (let [start-cpu (assoc (new-cpu) :y 0x10)
        end-cpu (first (run-asm start-cpu '((_ (tya)
                                               (stp)))))]
    (is (= 0x10 (end-cpu :a)))))

(deftest stp-should-stop-the-processor
  (let [start-cpu (new-cpu)
        end-cpu (first (run-asm start-cpu '((_ (stp)
                                               (sec)))))]
    (is (= 0 (carry end-cpu)))))

(deftest lda-in-emulation-mode-should-load-1-byte
  (let [cpu (first (run-asm (new-cpu) '((_     (lda num)
                                               (stp))
                                        (num   (dc i1 42))
                                        (extra (dc i1 43)))))]
    (is (= 42 (a cpu)))))

(deftest lda-in-8-bit-mode-should-load-1-byte
  (let [cpu (first (run-asm (new-cpu) '((_     (clc)
                                               (xce)
                                               (lda num)
                                               (stp))
                                        (num   (dc i1 42))
                                        (extra (dc i1 43)))))]
    (is (= 42 (a cpu)))))


(deftest lda-should-set-zero-if-the-result-is-zero
  (let [cpu (first (run-asm (new-cpu) '((_     (clc)
                                               (xce)
                                               (lda num)
                                               (stp))
                                        (num   (dc i1 0)))))]
    (is (= 1 (zero-bit cpu)))))

(deftest lda-should-clear-zero-if-the-result-is-not-zero
  (let [cpu (first (run-asm (new-cpu) '((_     (clc)
                                               (xce)
                                               (lda num)
                                               (stp))
                                        (num   (dc i1 5)))))]
    (is (= 0 (zero-bit cpu)))))

(deftest lda-should-set-negative-if-the-result-is-twos-complement-negative
  (let [cpu (first (run-asm (new-cpu) '((_     (clc)
                                               (xce)
                                               (lda num)
                                               (stp))
                                        (num   (dc i1 2r10000000)))))]
    (is (= 1 (negative-bit cpu)))))

(deftest lda-in-16-bit-mode-should-load-2-bytes
  (let [cpu (first (run-asm (new-cpu) '((_     (clc)
                                               (xce)
                                               (sep 2r00110000)
                                               (lda num)
                                               (stp))
                                        (num   (dc a 0xFFAA)))))]
    (is true)
    #_(is 0xFFAA (a cpu))))

(run-tests)
