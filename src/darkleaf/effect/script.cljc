(ns darkleaf.effect.script
  (:refer-clojure :exclude [test])
  (:require
   [clojure.test :as t]
   [clojure.string :as str]
   [clojure.data :as data]
   [matcher-combinators.parser]
   [matcher-combinators.core :as m]
   [darkleaf.effect.internal :as i]))

(defn- with-exceptions [continuation]
  (when (some? continuation)
    (fn [coeffect]
      (try
        (let [[effect continuation] (continuation coeffect)
              continuation          (with-exceptions continuation)]
          [effect continuation])
        (catch #?(:clj RuntimeException, :cljs js/Error) ex
          [ex nil])))))

(defn- matcher-report [matcher actual]
  (let [{:matcher-combinators.result/keys [value] :as result}
        (m/match matcher actual)]
    (if-not (m/match? result)
      {:type     :fail
       :expected (list 'match? matcher actual)
       :actual   value})))

(defn- test-first-item [{:keys [report continuation]} {:keys [args]}]
  (let [[effect continuation] (continuation args)]
    {:report        report
     :actual-effect effect
     :continuation  continuation}))

(defn- next-step [{:keys [report continuation]} coeffect]
  (let [[actual-effect continuation] (continuation coeffect)]
    {:report        report
     :actual-effect actual-effect
     :continuation  continuation}))

(defn- test-middle-item [{:keys [report actual-effect continuation] :as ctx}
                         {:keys [effect coeffect] :as item}]
  (i/<<-
   (if (not= :pass (:type report))
     {:report report})

   (if (nil? continuation)
     {:report {:type     (if (i/throwable? actual-effect) :error :fail)
               :expected effect
               :actual   actual-effect
               :message  "Unexpected return. An effect is expected."}})

   (if (contains? item :effect)
     (if-some [report (matcher-report effect actual-effect)]
       {:report (assoc report :message "Wrong effect")}
       (next-step ctx coeffect)))

   {:report {:type     :fail
             :expected '(contains? script-item :effect)
             :actual   item
             :message  "Wrong script item"}}))

(defn- test-middle-items [ctx items]
  (reduce test-middle-item ctx items))

(defn- test-last-item [{:keys [report actual-effect continuation]}
                       {:keys [return final-effect thrown] :as item}]
  (i/<<-
   (if (not= :pass (:type report))
     {:report report})

   (if (contains? item :final-effect)
     (if (some? continuation)
       (if-some [report (matcher-report final-effect actual-effect)]
         {:report (assoc report :message "Wrong final effect")}
         {:report report})
       {:report {:type     :fail
                 :expected '(some? continuation)
                 :actual   actual-effect
                 :message  "The function returned a value"}}))

   (if (contains? item :thrown)
     (if-some [report (matcher-report thrown actual-effect)]
       {:report (assoc report :message "Wrong exception")}
       {:report report}))

   (if (some? continuation)
     {:report {:type     :fail
               :expected nil
               :actual   actual-effect
               :message  "Extra effect"}})

   (if (contains? item :return)
     (if-some [report (matcher-report return actual-effect)]
       {:report (assoc report :message "Wrong return")}
       {:report report}))

   {:report {:type     :fail
             :expected '(or (contains? script-item :return)
                            (contains? script-item :final-effect)
                            (contains? script-item :thrown))
             :actual   item
             :message  "Wrong script item"}}))

(defn test* [continuation script]
  {:pre [(<= 2 (count script))]}
  (let [first-item   (first script)
        middle-items (-> script rest butlast)
        last-item    (last script)
        continuation (-> continuation
                         (with-exceptions))]
    (-> {:continuation continuation, :report {:type :pass}}
        (test-first-item first-item)
        (test-middle-items middle-items)
        (test-last-item last-item)
        :report)))

(defn test [continuation script]
  (-> (test* continuation script)
      (t/do-report)))
