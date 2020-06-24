(ns darkleaf.effect.script
  (:refer-clojure :exclude [test])
  (:require
   [clojure.test :as t]
   [clojure.string :as str]
   [clojure.data :as data]
   [darkleaf.effect.core :as core]
   [darkleaf.effect.internal :as i]))

(defn- match-value [expected actual]
  (when (not= expected actual)
    (if (i/throwable? actual)
      {:type     :error
       :expected expected
       :actual   actual}
      {:type     :fail
       :expected expected
       :actual   actual
       :diffs    [[actual (data/diff expected actual)]]})))

(defn- match-throwable [expected actual]
  (i/<<-
   (if-not (map? expected)
     {:type     :fail
      :expected (list 'map? expected)
      :actual   false})
   (if-not (i/throwable? actual)
     {:type     :fail
      :expected expected
      :actual   actual})
   (let [actual-as-data {:type    (type actual)
                         :message (ex-message actual)
                         :data    (ex-data actual)}])
   (if-not (= expected actual-as-data)
     {:type     :fail
      :expected expected
      :actual   actual-as-data
      :diffs    [[actual-as-data (data/diff expected actual-as-data)]]})))

(defn- with-exceptions [continuation]
  (reify
    core/Continuation
    (done? [_] (core/done? continuation))
    (run [_ coeffect]
      (try
        (core/run continuation coeffect)
        (catch #?(:clj RuntimeException, :cljs js/Error) ex
          ex)))))

(defn- test-first-item [{:keys [report continuation]} {:keys [args]}]
  (let [effect (core/run continuation args)]
    {:report        report
     :actual-effect effect
     :continuation  continuation}))

(defn- next-step [{:keys [report continuation]} coeffect]
  (let [actual-effect (core/run continuation coeffect)]
    {:report        report
     :actual-effect actual-effect
     :continuation  continuation}))

(defn- test-middle-item [{:keys [report actual-effect continuation] :as ctx}
                         {:keys [effect coeffect] :as item}]
  (i/<<-
   (if (not= :pass (:type report))
     {:report report})

   (if (core/done? continuation)
     (if (i/throwable? actual-effect)
       {:report {:type     :error
                 :expected effect
                 :actual   actual-effect
                 :message  "Unexpected exception. An effect is expected."}}
       {:report {:type     :fail
                 :expected effect
                 :actual   actual-effect
                 :message  "Unexpected return. An effect is expected."}}))

   (if (contains? item :effect)
     (if-some [report (match-value effect actual-effect)]
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
     (if (not (core/done? continuation))
       (if-some [report (match-value final-effect actual-effect)]
         {:report (assoc report :message "Wrong final effect")}
         {:report report})
       (if (i/throwable? actual-effect)
         {:report {:type     :error
                   :expected '(some? continuation)
                   :actual   actual-effect
                   :message  "An exception was unexpectedly thrown"}}
         {:report {:type     :fail
                   :expected '(some? continuation)
                   :actual   actual-effect
                   :message "A value was unexpectedly returned"}})))

   (if (contains? item :thrown)
     (if-some [report (match-throwable thrown actual-effect)]
       {:report (assoc report :message "Wrong exception")}
       {:report report}))

   (if (not (core/done? continuation))
     {:report {:type     :fail
               :expected nil
               :actual   actual-effect
               :message  "Extra effect"}})

   (if (contains? item :return)
     (if-some [report (match-value return actual-effect)]
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
