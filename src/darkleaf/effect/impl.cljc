(ns darkleaf.effect.impl
  (:require
   [darkleaf.effect.proto :as p]
   [cloroutine.core :refer [cr]]

   [darkleaf.effect.deque :as deque]
   [darkleaf.effect.core :as e]))

(defrecord Effect [tag args])

(def interrupted-exception
  (InterruptedException. "Interrupted coroutine"))

(declare ->Continuation)

(defn body->continuation [breaker body]
  `(let [done?#       (atom false)
         effect#      (atom nil)
         coeffect-fn# (atom (fn [] nil))
         return#      (atom nil)
         resume#      (fn [] (@coeffect-fn#))
         coroutine#   (cr {~breaker resume#}
                          (try
                            ~@body
                            (catch InterruptedException ex#
                              (reset! effect# @return#)
                              @return#)
                            (finally
                              (reset! done?# true))))]
     (reset! effect# (coroutine#))
     (->Continuation done?# effect# coeffect-fn# return# coroutine#)))

(deftype Continuation [done? effect coeffect-fn return coroutine]
  p/Continuation
  (done? [_] @done?)
  (effect [_] @effect)
  (next [_ coeffect]
    (reset! coeffect-fn (fn [] coeffect))
    (reset! effect (coroutine))
    nil)
  (throw [_ throwable]
    (reset! coeffect-fn (fn [] (throw throwable)))
    (reset! effect (coroutine))
    nil)
  (return [_ value]
    (reset! return value)
    (reset! coeffect-fn (fn [] (throw interrupted-exception)))
    (reset! effect (coroutine))
    nil))

(defn- join [deque]
  (let [continuation (deque/peek-first deque)
        effect       (p/effect continuation)]
    (if (p/done? continuation)
      (cond
        ;; tail call optimization
        (instance? Continuation effect)
        (do (deque/poll-first deque)
            (deque/add-first deque effect)
            nil)
        :else nil)
      (cond
        (instance? Effect effect)
        nil
        (instance? Continuation effect)
        (do (deque/add-first deque effect)
            nil)
        :else
        (do
          (p/next continuation effect)
          (recur deque))))))

(deftype Wrapper [deque]
  p/Continuation
  (done? [this]
    (-> deque (deque/peek-first) (p/done?)))
  (effect [this]
    (-> deque (deque/peek-first) (p/effect)))
  (next [this coeffect]
    (-> deque (deque/peek-first) (p/next coeffect))
    (join deque))
  (throw [this throwable]
    (let [continuation (deque/peek-first deque)
          throwable    (try
                         (p/throw continuation throwable)
                         nil
                         (catch Exception ex ex))]
      (when (some? throwable)
        (if (= 1 (count deque))
          (throw throwable))
        (deque/poll-first deque)
        (recur throwable))))
  (return [this value]
    (let [continuation (deque/peek-first deque)]
      (p/return continuation value)
      (when (< 1 (count deque))
        (deque/poll-first deque)
        (recur value)))))

(defn wrapper [continuation]
  (let [deque (deque/deque)]
    (deque/add-first deque continuation)
    (join deque)
    (->Wrapper deque)))
