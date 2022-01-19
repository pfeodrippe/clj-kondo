(ns pitoco.schema
  (:require
   [malli.instrument :as mi]
   [malli.dev]
   [malli.core :as m]
   [spec-provider.stats :as stats]
   [malli.clj-kondo :as mc]))

(defn vars
  []
  (->> (all-ns)
       (filter #(= (str %) "clj-kondo.impl.analyzer"))
       (mapv ns-publics)
       (mapcat vals))
  #_(->> (all-ns)
       (filter #(str/starts-with? % "clj-kondo."))
       (mapv ns-publics)
       (mapcat vals)
       (filter #(-> % meta :s-auto))))

(defn instrument!
  [{:keys [mode]}]
  (doseq [v (vars)]
    (case mode
      :instrument
      (let [original-fn (or (::original-fn (meta v))
                            (deref v))
            input-output* (atom [])]
        (when (fn? original-fn)
          (alter-meta! v assoc
                       ::original-fn original-fn
                       ::input-output input-output*)
          (alter-var-root v (constantly (fn [& args]
                                          (let [value (apply original-fn args)]
                                            (swap! input-output* conj {:input args
                                                                       :output value})
                                            value))))))

      :unstrument
      (when-let [original-fn (::original-fn (meta v))]
        (alter-meta! v dissoc ::original-fn ::input-output :malli/schema)
        (alter-var-root v (constantly original-fn))))))

(def ^:private original-preds
    stats/preds)

  (defn collect-stats
    [samples {existing-data-formats ::data-formats
              existing-stats ::stats :as options}]
    (binding [stats/preds (concat original-preds (mapv m/validator existing-data-formats))]
      (reduce (fn [stats x]
                (stats/update-stats stats x options))
              existing-stats
              samples)))

  (defn infer-schema
    "Infer schema from a collection of samples.
  `:symbol-format?` is `true` by default, it makes all the schemas symbols.
  Set it to `falsy` if you want the output in the form of preds/schemas."
    ([samples]
     (infer-schema samples {}))
    ([samples {:keys [:symbol-format? :debug? ::schemas]
               existing-data-formats ::data-formats
               existing-stats ::stats
               :or {symbol-format? true
                    schemas []}
               :as options}]
     ;; We cannot have `existing-stats` set and not have the `existing-data-formats`
     ;; are not serializable and each new evaluation here (from `new-data-formats`)
     ;; would return different values. We will try to deal with it in the future.
     (when (and existing-stats (not existing-data-formats))
       (throw (ex-info (str "`existing-data-formats` should have a value when `existing-stats` "
                            "is not `nil`.")
                       {:existing-stats existing-stats
                        :existing-data-formats existing-data-formats})))
     (let [data-formats' (or existing-data-formats #_(new-data-formats schemas))
           ;; `data-formats` needs to be exactly the same to match correctly as
           ;; they use the preds for matching, which are just functions which
           ;; are created on the fly (not equal as each evaluation returns a
           ;; different function). We could work in the future to make them
           ;; serializable using `malli` itself (with `sci`).
           stats (collect-stats samples (assoc options ::data-formats data-formats'))
           ;; `itself` data formats are the ones which check to themselves. They
           ;; are used to represent keys in a map so you can have a less generic
           ;; type if all of the map keys are `itself`.
           itself-value? (fn [v]
                           (->> data-formats'
                                (filter #(-> % m/type-properties ::itself?))
                                (mapv #(m/validate % v))
                                (some true?)))
           validators (set (mapv m/validator data-formats'))
           ;; TODO: Let's try to make this as deterministic as possible
           ;; so we can compare schema data directly without having to
           ;; define a powerful form of equivalence.
           schema
           (fn schema [stats]
             ;; `rs` is a function which checks if we want to output a symbol
             ;; or a resolved function (the last serves better for generation
             ;; of samples and for equality in tests without having to use a
             ;; registry).
             (let [rs (memoize (fn [v]
                                 (if symbol-format?
                                   (if debug?
                                     ;; Attach metadata to each element.
                                     (with-meta v {:stats stats})
                                     v)
                                   (-> v resolve deref))))
                   ;; We remove the preds which have a lower hierarchy (custom ones
                   ;; always are in a higher level).
                   invalid-original-validators (->> (::stats/pred-map stats)
                                                    (remove (comp (conj validators map? set? sequential?) first))
                                                    (filter (fn [[spec-type]]
                                                              (->> (::stats/distinct-values stats)
                                                                   (filter spec-type)
                                                                   (every? #(some (fn [type] (m/validate type %))
                                                                                  data-formats')))))
                                                    set)
                   types' (->> (::stats/pred-map stats)
                               (remove invalid-original-validators)
                               (mapv (fn [[spec-type]]
                                       (let [data-format (delay (->> data-formats'
                                                                     (filter (comp #{spec-type} m/validator))
                                                                     first))
                                             res (delay (condp = spec-type
                                                          map? (if (and (or (some-> stats
                                                                                    ::stats/map
                                                                                    ::stats/non-keyword-sample-count
                                                                                    pos?)
                                                                            (some-> stats
                                                                                    ::stats/map
                                                                                    ::stats/mixed-sample-count
                                                                                    pos?))
                                                                        ;; If all preds are part of
                                                                        ;; a schema which checks itself,
                                                                        ;; (ignoring keywords), then
                                                                        ;; we have a one to one mapping
                                                                        ;; and we can use these schemas
                                                                        ;; as keys (no need for `:map-of`.
                                                                        (->> (get-in stats [::stats/map ::stats/keys])
                                                                             keys
                                                                             (remove keyword?)
                                                                             (every? itself-value?)
                                                                             not))
                                                                 ;; If we have some non keyword key,
                                                                 ;; we move to a more generic map
                                                                 ;; using `:map-of`.
                                                                 [:map-of
                                                                  (infer-schema
                                                                   (->> (get-in stats [::stats/map ::stats/keys])
                                                                        keys
                                                                        ;; Convert any keyword to string
                                                                        ;; to be in sync with the JSON
                                                                        ;; format.
                                                                        (mapv #(if (keyword? %) (name %) %)))
                                                                   ;; We remove `::stats` here as
                                                                   ;; we want a clean slate for these.
                                                                   (dissoc options ::stats))
                                                                  (let [map-of-types (some->>
                                                                                      (get-in stats [::stats/map ::stats/keys])
                                                                                      vals
                                                                                      (mapv #(schema %))
                                                                                      set)]
                                                                    (if (> (count map-of-types) 1)
                                                                      ;; Here we could have nested `:or`s
                                                                      ;; which could be simplified, but
                                                                      ;; not a priority now.
                                                                      (into [:or] (sort-by str map-of-types))
                                                                      (first map-of-types)))]
                                                                 (->> (get-in stats [::stats/map ::stats/keys])
                                                                      (mapv (fn [[k nested-stats]]
                                                                              ;; If some key has less samples
                                                                              ;; than the count of maps, then
                                                                              ;; this is a optional key.
                                                                              (if (< (::stats/sample-count nested-stats)
                                                                                     (get-in stats [::stats/map ::stats/sample-count]))
                                                                                [k {:optional true} (schema nested-stats)]
                                                                                [k (schema nested-stats)])))
                                                                      (sort-by first)
                                                                      (into [:map])))
                                                          string? (rs 'string?)
                                                          integer? (rs 'int?)
                                                          set? [:set (schema (::stats/elements-set stats))]
                                                          sequential? [:sequential (schema (::stats/elements-coll stats))]
                                                          nil? (rs 'nil?)
                                                          stats/float? (rs 'number?)
                                                          stats/double? (rs 'number?)
                                                          decimal? (rs 'decimal?)
                                                          number? (rs 'number?)
                                                          boolean? (rs 'boolean?)
                                                          inst? (rs 'inst?)
                                                          symbol? (rs 'symbol?)
                                                          keyword? (rs 'keyword?)
                                                          nil))]
                                         (cond
                                           @res
                                           @res

                                           @data-format
                                           (m/type @data-format)

                                           :else
                                           (rs 'any?))))))
                   types (remove #{(rs 'any?) (rs 'nil?)} types')]
               (cond
                 (zero? (count types'))
                 (rs 'any?)

                 ;; Convert `nil?` to `any?` as
                 ;; it's very likely that a parameter
                 ;; is not really only `nil`, it's only
                 ;; that we are not testing all the
                 ;; possible cases.
                 (= (set types') #{(rs 'nil?)})
                 (rs 'any?)

                 (= (count types') 1)
                 (first types')

                 (= (set types') #{(rs 'any?) (rs 'nil?)})
                 (rs 'any?)

                 (some #{(rs 'nil?)} types')
                 [:maybe
                  (if (= (count types) 1)
                    ;; When there is some `any?` together some other types, we can
                    ;; get rid of the any.
                    (first types)
                    (into [:or] (sort-by str types)))]

                 :else
                 (if (= (count types) 1)
                   (first types)
                   (into [:or] (sort-by str types))))))]
       (schema stats))))

(defn transpose [m]
  (apply mapv vector m))

(defn infer-schemas!
  []
  (->> (vars)
       (filter #(some-> (meta %) ::input-output deref seq))
       (mapv (fn [v]
               (try
                 (let [input-output (some-> (meta v) ::input-output deref)
                       schema (into [:function]
                                    (->> (group-by (comp count :input) input-output)
                                         (mapv val)
                                         (mapv (fn [per-arity]
                                                 [:=>
                                                  (->> (transpose (mapv :input per-arity))
                                                       (mapv (fn [arg-values]
                                                               (infer-schema arg-values)))
                                                       (into [:cat]))
                                                  (infer-schema (mapv :output per-arity))]))))]
                   (alter-meta! v assoc :malli/schema schema)
                   #_(alter-meta! v update :doc str "\n\n\nInferred Schema:\n" (with-out-str
                                                (clojure.pprint/pprint schema)))
                   (mi/collect! {:ns (:ns (meta v))})
                   [v :ok])
                 (catch Exception e
                   [v e]))))))

(comment

  (instrument! {:mode :instrument})
  (infer-schemas!)
  (instrument! {:mode :unstrument})
  (mi/instrument!)
  (mi/unstrument!)

  (-> (mc/collect) (mc/linter-config))
  (mc/emit!)

  (:malli/schema (meta (var clj-kondo.impl.analyzer/analyze-defn)))
  (:doc (meta (var clj-kondo.impl.analyzer/analyze-defn)))

  (infer-schema [{:a 3} {:b 4 :a 4}])

  ())
