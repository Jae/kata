(ns ^{:doc "http://codekata.com/kata/kata01-supermarket-pricing"}
  kata.ex01)

(defmulti unit-adjusted
  (fn [{to-unit :unit} {from-unit :unit}] [from-unit to-unit]))

(defmethod unit-adjusted [:gram :ounce]
  [_ {from-price :price from-quantity :quantity :as from}]
  (assoc from
    :unit :ounce
    :quantity 1
    :price (* (/ from-price from-quantity) 28.3495)))

(defmethod unit-adjusted :default
  [{to-unit :unit} {from-unit :unit :as from}]
  (when-not (= from-unit to-unit)
    (throw (Exception. (str "unit conversion from " from-unit " to " to-unit " is not defined."))))
  from)

(defn- apply-saving
  [item saving]
  (unit-adjusted item saving))

(defn- apply-price
  [{item-quantity :quantity :as item} price]
  (let [{unit-price :price unit-quantity :quantity :as price} (unit-adjusted item price)]
    (assoc price
      :quantity item-quantity
      :price (* item-quantity (/ unit-price unit-quantity)))))

(defn- price-breakdown
  ([price savings item]
   (price-breakdown price savings item []))
  ([price savings {item-quantity :quantity :as item} subtotal]
   (if (= 0 item-quantity)
     subtotal
     (if (empty? savings)
       (conj subtotal (apply-price item price))
       (let [[{saving-quantity :quantity :as saving}] savings
             without-saving (price-breakdown price (rest savings) item subtotal)]
         (if (< item-quantity saving-quantity)
           without-saving
           (let [item (assoc item :quantity (- item-quantity saving-quantity))
                 subtotal (conj subtotal (apply-saving item saving))
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
