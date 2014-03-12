(ns superclj.65816-test
  (:require [clojure.test :refer :all]
            [superclj.65816 :refer :all]
            [superclj.asm :as asm]))

(def c (new-cpu))

(defn with-carry-set [cpu]
  (assoc-in cpu [:registers :status] (bit-set (status cpu) 0)))

(defn with-carry-cleared [cpu]
  (assoc-in cpu [:registers :status] (bit-clear (status cpu) 0)))

(deftest clc-should-clear-carry
  (let [start-cpu (with-carry-set c)
        end-cpu (first (run start-cpu
                            (asm/asm '((clc)))))]
    (is (= 0 (carry end-cpu)))))

(deftest sec-should-set-carry
  (let [start-cpu (with-carry-cleared c)
        end-cpu (first (run start-cpu
                            (asm/asm '((sec)))))]
    (is (= 1 (carry end-cpu)))))

(deftest xce-should-exchange-carry-and-emulation-bits
  (let [start-cpu (with-carry-cleared c)
        end-cpu (first (run start-cpu
                            (asm/asm '((xce)))))]
    (is (= 0 (:emulation-mode end-cpu)))
    (is (= 1 (carry end-cpu)))))

(run-tests)
