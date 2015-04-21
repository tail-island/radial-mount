(ns radial-mount.common)

(defn dissoc-nil-value-entries
  [map]
  (not-empty (into {} (remove (comp nil? second) map))))
  
