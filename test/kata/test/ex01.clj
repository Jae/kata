(ns kata.test.ex01
  (:require [kata.ex01 :refer :all]
            [clojure.test :refer :all]))

(defn buy-3
  [item price]
  (assoc item :quantity 3 :price price :description (format "buy 3 for £%.2f" price)))

(defn buy-3-get-1-free
  [{price :price :as item}]
  (assoc item :quantity 4 :price (* 3 price) :description "buy 3 get 1 free"))

(defn multiple-of
  [{:keys [quantity price] :as item} multiple]
  (assoc item :quantity (* quantity multiple) :price (* price multiple)))

(def can-of-beer
  {:sku :can-of-beer
   :unit :ea
   :quantity 1
   :price 0.40
   :description "£0.40 each"})

(def loose-apple
  {:sku :loose-apple
   :unit :ea
   :quantity 1
   :price 0.45
   :description "£0.45 each"})

(def greens
  {:sku :greens
   :unit :gram
   :quantity 100
   :price 1.10
   :description "£1.10 per 100 grams"})

(def prices [can-of-beer loose-apple greens])

(def savings [(buy-3 can-of-beer 1.00) (buy-3 can-of-beer 1.65) (buy-3-get-1-free loose-apple)])

(deftest single-item
  (is (= {:items [can-of-beer] :total 0.40}
         (checkout [can-of-beer] prices savings))))

(deftest multiple-items
  (is (= {:items [(multiple-of can-of-beer 2)], :total 0.80}
         (checkout [(assoc can-of-beer :quantity 2)] prices savings))))

(deftest multi-buy-items
  (is (= {:items [(buy-3 can-of-beer 1.00)] :total 1.00}
         (checkout [(assoc can-of-beer :quantity 3)] prices savings))))

(deftest mixed-pricing
  (is (= {:items [(multiple-of (buy-3 can-of-beer 1.00) 2) can-of-beer] :total 2.40}
         (checkout [(assoc can-of-beer :quantity 7)] prices savings))))

(deftest mixed-items
  (is (= {:items [(multiple-of (buy-3 can-of-beer 1.00) 2) can-of-beer
                  (buy-3-get-1-free loose-apple) loose-apple], :total 4.20}
         (checkout [(assoc can-of-beer :quantity 7)
                    (assoc loose-apple :quantity 5)] prices savings))))

(deftest mixed-unit
  (is (= {:items [(assoc greens :unit :ounce :quantity 3 :price (* 3 28.3495 (/ (:price greens) (:quantity greens))))] :total (* 3 28.3495 (/ (:price greens) (:quantity greens)))}
         (checkout [(assoc greens :quantity 3 :unit :ounce)] prices savings))))
