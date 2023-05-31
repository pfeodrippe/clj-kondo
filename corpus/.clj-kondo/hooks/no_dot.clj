(ns hooks.no-dot
  (:require
   [clj-kondo.hooks-api :as api]
   [clojure.string :as str]))

;; This is an example of a hook. It complains
;; if the body of a call has a `.`.
(defn no-dot
  [{:keys [node]}]
  (let [children (rest (:children node))]
    (if (and (= (count children) 1)
             (api/string-node? (first children)))
      (let [s (api/sexpr (first children))]
        (when-let [idx (str/index-of s ".")]
          (let [newline-idx (str/last-index-of (subs s 0 idx) "\n")
                col (if newline-idx
                      (- idx newline-idx)
                      (+ idx (:col (meta node))))
                row (if newline-idx
                      (+ (get (frequencies (subs s 0 idx)) \newline)
                         (:row (meta node)))
                      (:row (meta node)))]
            (api/reg-finding! (assoc (meta node)
                                     :col col
                                     :row row
                                     :message (str "No `.` allowed")
                                     :type :linter/no-dot-string)))))
      (api/reg-finding! (assoc (meta node)
                               :message "Should be a string"
                               :type :linter/no-dot-string))))
  node)

(defn def*
  [{:keys [node]}]
  #_(println (meta (second (:children node))))
  (prn '(keys node) '-> (keys node))
  (prn '(keys (meta node)) '-> (keys (meta node)))
  (prn '(mapv meta (:children node)) '-> (mapv meta (:children node)))
  (prn '(mapv keys (:children node)) '-> (mapv :meta (:children node)))
  #_(throw (ex-info (pr-str (meta (second (:children node)))) {})))
