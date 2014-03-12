(ns superclj.memory-test
  (:require [clojure.test :refer :all]
            [superclj.memory :refer :all]))

(defn am []
  (allocate-memory 0xFFFF))

(deftest test-byte
  (let [memory (store-byte (am) 0x2000 0x1)]
    (is (= 0x1 (load-byte memory 0x2000)))))

(deftest test-bad-byte
  (let [memory (am)]
    (is (thrown? IllegalArgumentException (store-byte memory 0x2000 -0x1)))
    (is (thrown? IllegalArgumentException (store-byte memory 0x2000 0x100)))))

(deftest test-double
  (let [memory (store-double (am) 0x2000 0x1234)]
    (is (= 0x1234 (load-double memory 0x10)))))

(deftest test-split-double
  (let [memory (store-double (am) 0x2000 0x1234)]
    (is (= 0x34 (load-byte memory 0x2000)))
    (is (= 0x12 (load-byte memory 0x2001)))))

(deftest test-bad-double
  (let [memory (am)]
    (is (thrown? IllegalArgumentException (store-double memory 0xFF -0x1)))
    (is (thrown? IllegalArgumentException (store-double memory 0xFF 0x10000)))))
