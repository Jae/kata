(ns kata.ex01)

(defn- apply-price-saving
  [{unit-price :price} {price :price :as saving}]
  (assoc saving :price (price unit-price)))

(defn- apply-unit-price
  [{item-quantity :quantity} {unit-price :price unit-quantity :quantity :as price}]
  (assoc price
    :quantity item-quantity
    :price (* unit-price (/ item-quantity unit-quantity))))

(defn- price-breakdown
  ([price savings item]
   (price-breakdown price savings item []))
  ([price savings {item-quantity :quantity :as item} subtotal]
   (if (= 0 item-quantity)
     subtotal
     (if (empty? savings)
       (conj subtotal (apply-unit-price item price))
       (let [[{saving-quantity :quantity :as saving}] savings
             without-saving (price-breakdown price (rest savings) item subtotal)]
         (if (< item-quantity saving-quantity)
           without-saving
           (let [item (assoc item :quantity (- item-quantity saving-quantity))
                 subtotal (conj subtotal (apply-price-saving price saving))
                 with-saving (price-breakdown price savings item subtotal)]
             (if (<= (reduce + (map :price with-saving)) (reduce + (map :price without-saving)))
               with-saving
               without-saving))))))))

(defn- bill-items
  [prices savings {item-sku :sku item-quantity :quantity :as item}]
  (let [filter-by-sku #(filter (comp (partial = item-sku) :sku) %)
        price (first (filter-by-sku prices))
        savings (->> savings
                     filter-by-sku
                     (sort-by :quantity >))]
    (->> (price-breakdown price savings item)
         (group-by identity)
         (map (fn [[{:keys [quantity price] :as k} v]]
                (assoc k
                  :quantity (* quantity (count v))
                  :price (* price (count v))))))))

(defn checkout
  [shopping-cart prices savings]
  (let [bill-items (->> shopping-cart
                   (group-by :sku)
                   (map (fn [[_ items]]
                          (assoc (first items)
                            :quantity (reduce + (map #(get % :quantity 1) items)))))
                   (mapcat (partial bill-items prices savings)))
        total (reduce + (map :price bill-items))]
    {:items bill-items
     :total total}))

(comment
  (do
    (def can-of-beer
      {:sku :can-of-beer
       :unit :ea})

    (def loose-apple
      {:sku :loose-apple
       :unit :ea})

    (def greens
      {:sku :greens
       :unit :gram})

    (def prices
      [(assoc can-of-beer :quantity 1 :price 0.40 :description "£0.40 each")
       (assoc loose-apple :quantity 1 :price 0.45 :description "£0.45 each")
       (assoc greens :quantity 100 :price 1.10 :description "£1.10 per 100 grams")])

    (def savings
      [(assoc can-of-beer :quantity 3 :price (fn [unit-price] 1.00) :description "buy 3 for £1.00")
       (assoc can-of-beer :quantity 5 :price (fn [unit-price] 1.65) :description "buy 5 for £1.65")
       (assoc loose-apple :quantity 4 :price (fn [unit-price] (* 3 unit-price)) :description "buy 3 get 1 free")])

    (checkout [(assoc can-of-beer :quantity 7)
               (assoc loose-apple :quantity 5)
               (assoc greens :quantity 250)]
              prices
              savings)))
