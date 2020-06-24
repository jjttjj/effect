(ns darkleaf.effect.core
  (:require
   [cloroutine.core :refer [cr]]
   [darkleaf.effect.internal :as i])
  #?(:cljs (:require-macros [darkleaf.effect.core :refer [with-effects]]))
  (:import
   [java.util Stack]))

(set! *warn-on-reflection* true)

(defprotocol Continuation
  (done? [this])
  (run [this coeffect])
  #_(clone [this]))

(defprotocol State
  (set-state [this state]))

(defprotocol ContinuationState
  (state-done? [this impl])
  (state-run [this impl coeffect]))

(deftype ContinuationImpl [^:unsynchronized-mutable state]
  Continuation
  (done? [this] (state-done? state this))
  (run [this coeffect] (state-run state this coeffect))
  State
  (set-state [this new-state]
    (set! state new-state)))

(deftype NewContinuationState [effn]
  ContinuationState
  (state-done? [_ _] false)
  (state-run [this impl args]
    (let [coroutine  (apply effn args)
          stack      (Stack.)
          _          (.push stack coroutine)
          used-state (->UsedContinuationState stack false)]
      (set-state impl used-state)
      (run impl ::not-used))))

(deftype UsedContinuationState [^Stack stack ^:unsynchronized-mutable done?]
  ContinuationState
  (state-done? [_ _] done?)
  (state-run [this _ coeffect]
    (try
      (loop [coeffect coeffect]
        (if (.empty stack)
          (do (set! done? true)
              coeffect)
          (let [coroutine (.peek stack)
                val       (i/with-coeffect coeffect coroutine)]
            (case (i/kind val)
              :effect       val
              :coroutine    (do (.push stack val)
                                (recur ::not-used))
              :return-value (do (.pop stack)
                                (recur (i/unwrap-value val)))
              (recur val)))))
      (catch #?(:clj Throwable, :cljs js/Error) ex
        (set! done? true)
        (throw ex)))))

(defn effect [tag & args]
  (-> (cons tag args)
      (i/with-kind :effect)))

(defn ! [x]
  x)

(defmacro with-effects [& body]
  `(i/with-kind
     (cr {! i/coeffect}
         (i/wrap-return-value
          (do ~@body)))
     :coroutine))

(defn continuation [effn]
  (let [state (->NewContinuationState effn)]
    (->ContinuationImpl state)))

(defn- exec-effect
  ([handlers [tag & args]]
   (let [handler (get handlers tag)]
     (if-not (ifn? handler) (throw (ex-info "The effect handler is not a function"
                                            {:handler handler :tag tag})))
     (try
       (apply handler args)
       (catch #?(:clj Throwable, :cljs js/Error) error
         error))))
  ([handlers [tag & args] respond raise]
   (let [handler (get handlers tag)]
     (if-not (ifn? handler)
       (raise (ex-info "The effect handler is not a function"
                       {:handler handler :tag tag}))
       (apply handler (concat args [respond respond]))))))

(defn perform
  ([handlers continuation coeffect-or-args]
   (loop [effect (run continuation coeffect-or-args)]
     (if (done? continuation)
       effect
       (recur (run continuation (exec-effect handlers effect))))))
  ([handlers continuation coeffect-or-args respond raise]
   (try
     (let [effect (run continuation coeffect-or-args)]
       (if (done? continuation)
         (respond effect)
         (exec-effect handlers effect
                      (fn [coeffect]
                        (perform handlers continuation coeffect
                                 respond raise))
                      raise)))
     (catch #?(:clj Throwable, :cljs js/Error) error
       (raise error)))))
