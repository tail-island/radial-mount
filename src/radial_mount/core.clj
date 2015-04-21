(ns radial-mount.core
  (:use     (radial-mount common))
  (:require (clojure      [string :as string])
            (twin-spar    [core   :as twin-spar])
            (dog-mission  [core   :as dog-mission])))

(dog-mission/conj-resource-bundle-namespace "radial-mount.message")

(defn- get-value
  [row x]
  (cond
    (coll? x) (vec (map #(get-value row %) x))
    (ifn?  x) (x row)
    :else     x))

(defn property-message-key
  [table-key property-key]
  (keyword (apply format "%s$%s" (map name [table-key property-key]))))

(defn- validate-unary
  [validate-key valid?-fn default-message-key database-schema table-key row & [row-errors]]
  (letfn [(normalize
            ([property-key value message-key] [property-key value message-key])
            ([property-key value]             (normalize property-key value default-message-key))
            ([property-key]                   (normalize property-key property-key)))]
    (->> (get-in database-schema [table-key :validations validate-key])
         (map #(apply normalize %))
         (reduce (fn [result [property-key value message-key]]
                   (let [value  (get-value row value)
                         valid? (or (and (nil? value)                    ; 値がnilの場合は検査が成り立たないので、正しいとみなします。
                                         (not= validate-key :presence))  ; ただしもちろん、必須チェックでない場合のみですけど。
                                    (valid?-fn value))]
                     (cond-> result
                       (not valid?) (update-in [property-key] #(vec (conj % [message-key (property-message-key table-key property-key)]))))))
                 row-errors))))

(defn- validate-binary
  [validate-key valid?-fn default-message-key database-schema table-key row & [row-errors]]
  (letfn [(normalize
            ([property-key value-1 value-2 message-key] [property-key value-1 value-2 message-key])
            ([property-key value-1 value-2]             (normalize property-key value-1 value-2 default-message-key))
            ([property-key         value-2]             (normalize property-key property-key value-2)))]
    (->> (get-in database-schema [table-key :validations validate-key])
         (map #(apply normalize %))
         (reduce (fn [result [property-key value-1 value-2 message-key]]
                   (let [[_ value-2 :as values] (map (partial get-value row) [value-1 value-2])
                         valid?                 (or (some nil? values)  ; 値がnilの場合は検査が成り立たないので、正しいとみなします。今回は2項なので、単項である必須チェックかどうかの確認は不要です。
                                                    (apply valid?-fn values))]
                     (cond-> result
                       (not valid?) (update-in [property-key] #(vec (conj % [message-key (property-message-key table-key property-key) value-2]))))))
                 row-errors))))

(def ^:private validates
  [(partial validate-unary  :by-function          identity                       :wrong-value)
   (partial validate-binary :exclusion            #(not (contains? (set %2) %1)) :cannot-be-one-of)
   (partial validate-binary :inclusion            #(contains? (set %2) %1)       :must-be-one-of)
   (partial validate-binary :format               #(re-find %2 %1)               :has-the-wrong-format)
   (partial validate-binary :larger-than-or-equal >=                             :must-be-larger-than-or-equal)
   (partial validate-binary :larger-than          >                              :must-be-larger-than)
   (partial validate-binary :less-than-or-equal   <=                             :must-be-less-than-or-equal)
   (partial validate-binary :less-than            <                              :must-be-less-than)
   (partial validate-unary  :presence             (complement nil?)              :must-be-present)])

;; TODO: dateやtimestampの場合の>=等の比較について、検討する。

(defn- format-errors
  [database-errors]
  (letfn [(format-property-error [property-error]
            (->> property-error
                 (map (partial dog-mission/translate))
                 (apply format)
                 (string/capitalize)))
          (format-property-errors [property-errors]
            (->> property-errors
                 (map format-property-error)))
          (format-row-errors [row-errors]
            (->> row-errors
                 (reduce-kv #(assoc %1 %2 (format-property-errors %3)) nil)))
          (format-table-errors [table-errors]
            (->> table-errors
                 (reduce-kv #(assoc %1 %2 (format-row-errors %3)) nil)))]
    (->> database-errors
         (reduce-kv #(assoc %1 %2 (format-table-errors %3)) nil))))

(defn validate
  [database-schema database]
  (letfn [(validate-row [table-key row]
            ((apply comp (map #(partial % database-schema table-key row) validates))))
          (validate-table [table-key table]
            (->> table
                 (reduce-kv #(assoc %1 %2 (validate-row table-key %3)) nil)
                 (dissoc-nil-value-entries)))]
    (->> (apply merge-with merge ((juxt twin-spar/get-inserted-rows twin-spar/get-modified-rows) database))
         (reduce-kv #(assoc %1 %2 (validate-table %2 %3)) nil)
         (format-errors)
         (dissoc-nil-value-entries))))
