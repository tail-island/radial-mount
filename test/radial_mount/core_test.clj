(ns radial-mount.core-test
  (:use     (radial-mount core))
  (:require (clojure      [pprint :refer :all]
                          [test   :refer :all])
            (clj-time     [coerce :as    time.coerce])
            (dog-mission  [core   :as    dog-mission])
            (twin-spar    [core   :as    twin-spar]))
  (:import  (java.util    Locale)))

(dog-mission/conj-resource-bundle-namespace "radial-mount.property-caption"
                                            "radial-mount.extra-message")

(def database-schema
  {:products   {:columns                   {:code                 {:type      :string}
                                            :name                 {:type      :string}
                                            :price                {:type      :decimal}
                                            :discount-max         {:type      :decimal}
                                            :discon-at            {:type      :timestamp}
                                            :x-type               {:type      :int}
                                            :y-type               {:type      :int}
                                            :z?                   {:type      :boolean}}
                :many-to-one-relationships {:category             {:table-key :categories}}
                :validations               {:presence             [[:code] [:name] [:price] [:category]]
                                            :larger-than          [[:discount-max 0.00M]]
                                            :larger-than-or-equal [[:price 1000.00M]]
                                            :less-than            [[:price 10000.00M]
                                                                   [:discount-max #(/ (:price %) 2M)]]
                                            :format               [[:code :code #"\A\d{3}-\d{4}\z" "please input %1$s like 123-4567."]]
                                            :inclusion            [[:x-type [1 2 3]]]
                                            :exclusion            [[:y-type [1 2 3]]]
                                            :by-function          [[:price #(= (mod (:price %) 2M) 0M)]]}}
   :categories {:columns                   {:name                 {:type      :string}}
                :many-to-one-relationships {:superior-category    {:table-key :categories}}
                :one-to-many-relationships {:inferior-categories  {:table-key :categories, :many-to-one-relationship-key :superior-category}
                                            :products             {:table-key :products,   :many-to-one-relationship-key :category}}
                :validations               {:presence             [[:name]]
                                            :less-than-or-equal   [[:products #(count (:products %)) 5 :max-product-count-in-a-category-must-be-less-than-or-equal]]}}})

(def ^:private rm-database
  (partial twin-spar/database database-schema))

(def ^:private row-keys
  (repeatedly twin-spar/new-key))

(defn- row-key
  [index]
  (nth row-keys index))

(deftest validate-presence
  (binding [dog-mission/*locale* (Locale. "ja")]
    (let [database (rm-database {:categories {(row-key 10) {:key (row-key 10), :name "c0", :superior-category-key nil}
                                              (row-key 11) {:key (row-key 11), :name "c1", :superior-category-key (row-key 10)}
                                              (row-key 12) {:key (row-key 12), :name "c2", :superior-category-key (row-key 10)}}
                                 :products   {(row-key 20) {:key (row-key 20), :code "000-0000", :name "p0", :price 1000.00M, :discount-max nil, :discon-at nil, :x-type nil, :y-type nil, :z? nil, :category-key (row-key 10)}
                                              (row-key 21) {:key (row-key 21), :code "000-0010", :name "p1", :price 1010.00M, :discount-max nil, :discon-at nil, :x-type nil, :y-type nil, :z? nil, :category-key (row-key 11)}
                                              (row-key 22) {:key (row-key 22), :code "000-0020", :name "p2", :price 1020.00M, :discount-max nil, :discon-at nil, :x-type nil, :y-type nil, :z? nil, :category-key (row-key 12)}}})]
      (is (nil? (validate database-schema database)))
      (is (= (validate database-schema
                       (-> database
                           (assoc-in [:categories (row-key 10) :name]         nil)
                           (assoc-in [:products   (row-key 20) :code]         nil)
                           (assoc-in [:products   (row-key 20) :discount-max] 0.00M)
                           (assoc-in [:products   (row-key 20) :price]        999.00M)
                           (assoc-in [:products   (row-key 21) :code]         "1234567")
                           (assoc-in [:products   (row-key 21) :price]        10000.00M)
                           (assoc-in [:products   (row-key 21) :discount-max] 5000.00M)
                           (assoc-in [:products   (row-key 21) :x-type]       4)
                           (assoc-in [:products   (row-key 21) :y-type]       1)
                           (assoc-in [:products   (row-key 23)]               {:code "000-0003", :name "p3", :price 1030.00M, :discount-max nil, :discon-at nil, :x-type nil, :y-type nil, :z? nil, :category-key (row-key 10)})
                           (assoc-in [:products   (row-key 24)]               {:code "000-0004", :name "p4", :price 1040.00M, :discount-max nil, :discon-at nil, :x-type nil, :y-type nil, :z? nil, :category-key (row-key 10)})
                           (assoc-in [:products   (row-key 25)]               {:code "000-0005", :name "p5", :price 1050.00M, :discount-max nil, :discon-at nil, :x-type nil, :y-type nil, :z? nil, :category-key (row-key 10)})
                           (assoc-in [:products   (row-key 26)]               {:code "000-0006", :name "p6", :price 1060.00M, :discount-max nil, :discon-at nil, :x-type nil, :y-type nil, :z? nil, :category-key (row-key 10)})
                           (assoc-in [:products   (row-key 27)]               {:code "000-0007", :name "p7", :price 1070.00M, :discount-max nil, :discon-at nil, :x-type nil, :y-type nil, :z? nil, :category-key (row-key 10)})
                           ))
             {:categories {(row-key 10) {:name         ["Name must be present."]
                                         :products     ["Max product count in a category must be less than or equal 5."]}}
              :products   {(row-key 20) {:code         ["Code must be present."]
                                         :price        ["Price must be larger than or equal 1,000.00."
                                                        "Price value is wrong."]
                                         :discount-max ["Discount limit must be larger than 0.00."]}
                           (row-key 21) {:code         ["Please input code like 123-4567."]
                                         :price        ["Price must be less than 10,000.00."]
                                         :discount-max ["Discount limit must be less than 5,000.00."]
                                         :x-type       ["X type must be one of [1 2 3]."]
                                         :y-type       ["Y type can't be one of [1 2 3]."]}}})))))
