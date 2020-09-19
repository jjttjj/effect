(ns darkleaf.effect.script
  (:refer-clojure :exclude [test])
  (:require
   [clojure.test :as t]
   [darkleaf.effect.proto :as p]
   ;; [clojure.string :as str]
   [clojure.data :as data]))
   ;; [darkleaf.effect.util :as u]))

(defmacro <<- [& body]
  `(->> ~@(reverse body)))


(defn- match-value [expected actual]
  (when (not= expected actual)
    {:type     :fail
     :expected expected
     :actual   actual
     :diffs    [[actual (data/diff expected actual)]]}))

;; (defn- match-throwable [expected actual]
;;   (u/<<-
;;    (if-not (map? expected)
;;      {:type     :fail
;;       :expected (list 'map? expected)
;;       :actual   false})
;;    (if-not (u/throwable? actual)
;;      {:type     :fail
;;       :expected expected
;;       :actual   actual})
;;    (let [actual-as-data {:type    (type actual)
;;                          :message (ex-message actual)
;;                          :data    (ex-data actual)}])
;;    (if-not (= expected actual-as-data)
;;      {:type     :fail
;;       :expected expected
;;       :actual   actual-as-data
;;       :diffs    [[actual-as-data (data/diff expected actual-as-data)]]})))

;; (defn- with-exceptions [continuation]
;;   (when (some? continuation)
;;     (fn [coeffect]
;;       (try
;;         (let [[effect continuation] (continuation coeffect)
;;               continuation          (with-exceptions continuation)]
;;           [effect continuation])
;;         (catch #?(:clj RuntimeException, :cljs js/Error) ex
;;           [ex nil])))))

(defn- test-first-item [{:keys [report ef]} {:keys [args]}]
  (try
    {:report       report
     :continuation (apply ef args)}
    #_(catch RuntimeException ex
        {:exception ex})))

;; (defn- next-step [{:keys [report continuation]} coeffect]
;;   (let [[actual-effect continuation] (continuation coeffect)]
;;     {:report        report
;;      :actual-effect actual-effect
;;      :continuation  continuation}))


(defn- test-middle-item [{:keys [report continuation] :as ctx}
                         {:keys [effect coeffect return] :as item}]
  (<<-
   (if (not= :pass (:type report))
     {:report report})

   (if (p/done? continuation)
     {:report {:type     :fail
               :expected effect
               :actual   (p/effect continuation)
               :message  "An unfinished continuation. An effect is expected."}})

   (if-not (contains? item :effect)
     {:report {:type     :fail
               :expected '(contains? script-item :effect)
               :actual   item
               :message  "A wrong script item"}})

   (if-some [report (match-value effect (p/effect continuation))]
     {:report (assoc report :message "A wrong effect")})

   (if (contains? item :coeffect)
     (do
       (p/next continuation coeffect)
       ctx))

   (if (contains? item :return)
     (do
       (p/return continuation return)
       ctx))

   {:report {:type :fail
             :message "AAAA"}}))


;; (defn- test-middle-item [{:keys [report actual-effect continuation] :as ctx}
;;                          {:keys [effect coeffect] :as item}]
;;   (u/<<-
;;    (if (not= :pass (:type report))
;;      {:report report})

;;    (if (nil? continuation)
;;      (if (u/throwable? actual-effect)
;;        {:report {:type     :error
;;                  :expected effect
;;                  :actual   actual-effect
;;                  :message  "Unexpected exception. An effect is expected."}}
;;        {:report {:type     :fail
;;                  :expected effect
;;                  :actual   actual-effect
;;                  :message  "Unexpected return. An effect is expected."}}))

;;    (if (contains? item :effect)
;;      (if-some [report (match-value effect actual-effect)]
;;        {:report (assoc report :message "Wrong effect")}
;;        (next-step ctx coeffect)))

;;    {:report {:type     :fail
;;              :expected '(contains? script-item :effect)
;;              :actual   item
;;              :message  "Wrong script item"}}))

(defn- test-middle-items [ctx items]
  (reduce test-middle-item ctx items))

(defn- test-last-item [{:keys [report continuation #_exception] :as ctx}
                       {:keys [value #_thrown] :as item}]
  (<<-
   ;; (if (contains? ctx :exception)
   ;;   (if (contains? item :thrown)
   ;;     (if-some [report (match-throwable thrown exception)]
   ;;       {:report (assoc report :message "A wrong exception")}
   ;;       {:report report})
   ;;     {:report {:type     :error
   ;;               :expected '(some? effect)
   ;;               :actual   exception
   ;;               :message  "An exception was unexpectedly thrown"}}))

   (if (not= :pass (:type report))
     {:report report})

   (if-not (p/done? continuation)
     {:report {:type     :fail
               :expected nil
               :actual   (p/effect continuation)
               :message  "An extra effect"}})

   (if (contains? item :value)
     (if-some [report (match-value value (p/effect continuation))]
       {:report (assoc report :message "A wrong value")}
       {:report report}))

   {:report {:type       :fail
             #_:expected #_' (or (contains? script-item :value)
                                 (contains? script-item :thrown))
             :actual     item
             :message    "A wrong script item"}}))





;; (defn- test-last-item [{:keys [report actual-effect continuation]}
;;                        {:keys [return final-effect thrown] :as item}]
;;   (u/<<-
;;    (if (not= :pass (:type report))
;;      {:report report})

;;    (if (contains? item :final-effect)
;;      (if (some? continuation)
;;        (if-some [report (match-value final-effect actual-effect)]
;;          {:report (assoc report :message "Wrong final effect")}
;;          {:report report})
;;        (if (u/throwable? actual-effect)
;;          {:report {:type     :error
;;                    :expected '(some? continuation)
;;                    :actual   actual-effect
;;                    :message  "An exception was unexpectedly thrown"}}
;;          {:report {:type     :fail
;;                    :expected '(some? continuation)
;;                    :actual   actual-effect
;;                    :message "A value was unexpectedly returned"}})))

;;    (if (contains? item :thrown)
;;      (if-some [report (match-throwable thrown actual-effect)]
;;        {:report (assoc report :message "Wrong exception")}
;;        {:report report}))

;;    (if (some? continuation)
;;      {:report {:type     :fail
;;                :expected nil
;;                :actual   actual-effect
;;                :message  "Extra effect"}})

;;    (if (contains? item :return)
;;      (if-some [report (match-value return actual-effect)]
;;        {:report (assoc report :message "Wrong return")}
;;        {:report report}))

;;    {:report {:type     :fail
;;              :expected '(or (contains? script-item :return)
;;                             (contains? script-item :final-effect)
;;                             (contains? script-item :thrown))
;;              :actual   item
;;              :message  "Wrong script item"}}))

(defn test* [ef script]
  {:pre [(<= 2 (count script))]}
  (let [first-item   (first script)
        middle-items (-> script rest butlast)
        last-item    (last script)]
        ;; continuation (-> continuation
        ;;                  (with-exceptions))]
    (-> {:ef ef, :report {:type :pass}}
        (test-first-item first-item)
        (test-middle-items middle-items)
        (test-last-item last-item)
        :report)))

(defn test [ef script]
  (-> (test* ef script)
      (t/do-report)))


;; (let [x #(identity {:x %})]
;;   (x 1))
