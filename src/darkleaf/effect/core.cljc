(ns darkleaf.effect.core
  (:require
   [darkleaf.effect.proto :as p]
   [cloroutine.core :refer [cr]]))
  ;;  [darkleaf.effect.internal :as i])
  ;; #?(:cljs (:require-macros [darkleaf.effect.core :refer [with-effects]])))

(defn effect [tag & args]
  (-> (cons tag args)
      #_(i/with-kind :effect)))

(defn ! [x]
  x)

(def -interrupted-exception
  (InterruptedException. "Interrupted coroutine"))

(defmacro with-effects [& body]
  `(let [done?#       (atom false)
         effect#      (atom nil)
         coeffect-fn# (atom (fn [] nil))
         return#      (atom nil)
         resume#      (fn [] (@coeffect-fn#))
         coroutine#   (cr {! resume#}
                          (try
                            ~@body
                            (catch InterruptedException ex#
                              (reset! effect# @return#)
                              @return#)
                            (finally
                              (reset! done?# true))))]
     (reset! effect# (coroutine#))
     (reify p/Continuation
       (done? [_] @done?#)
       (effect [_] @effect#)
       (next [_ coeffect#]
         (reset! coeffect-fn# (fn [] coeffect#))
         (reset! effect# (coroutine#))
         nil)
       (throw [_ throwable#]
         (reset! coeffect-fn# (fn [] (throw throwable#)))
         (reset! effect# (coroutine#))
         nil)
       (return [_ value#]
         (reset! return# value#)
         (reset! coeffect-fn# (fn [] (throw -interrupted-exception)))
         (reset! effect# (coroutine#))
         nil)
       (clone [_]
         (coroutine# identity)))))

#_(let [cont (with-effects
               (try
                 (! :a)
                 (throw (ex-info "aa" {}))
                 (finally
                   (! :b))))]

    (loop []
      (prn (p/effect cont))
      (if (p/done? cont)
        (do
          (prn :done)
          :done)
        (do
          (try
            (p/next cont 1)
            (catch Throwable ex
              (prn (ex-message ex))))
          (recur)))))




;; (defmacro with-effects [& body]
;;   `(i/with-kind
;;      (cr {! i/coeffect}
;;          (i/wrap-return-value
;;           (do ~@body)))
;;      :coroutine))

;; (defn- update-head [coll f & args]
;;   (if (seq coll)
;;     (-> coll
;;         (pop)
;;         (conj (apply f (peek coll) args)))
;;     coll))

;; (defn- clone-coroutine [coroutine]
;;   (coroutine identity))

;; (defn- stack->continuation [stack]
;;   (fn [coeffect]
;;     (loop [stack    (update-head stack clone-coroutine)
;;            coeffect coeffect]
;;       (if (empty? stack)
;;         [coeffect nil]
;;         (let [coroutine (peek stack)
;;               val       (i/with-coeffect coeffect coroutine)]
;;           (case (i/kind val)
;;             :effect       [val (stack->continuation stack)]
;;             :coroutine    (recur (conj stack val) ::not-used)
;;             :return-value (recur (pop stack) (i/unwrap-value val))
;;             (recur stack val)))))))

;; (defn continuation [effn]
;;   (fn [args]
;;     (let [coroutine (apply effn args)
;;           stack     (list coroutine)
;;           cont      (stack->continuation stack)
;;           coeffect  ::not-used]
;;       (cont coeffect))))

;; (defn- exec-effect
;;   ([handlers [tag & args]]
;;    (let [handler (get handlers tag)]
;;      (if-not (ifn? handler) (throw (ex-info "The effect handler is not a function"
;;                                             {:handler handler :tag tag})))
;;      (try
;;        (apply handler args)
;;        (catch #?(:clj Throwable, :cljs js/Error) error
;;          error))))
;;   ([handlers [tag & args] respond raise]
;;    (let [handler (get handlers tag)]
;;      (if-not (ifn? handler)
;;        (raise (ex-info "The effect handler is not a function"
;;                        {:handler handler :tag tag}))
;;        (apply handler (concat args [respond respond]))))))

;; (defn perform
;;   ([handlers continuation coeffect-or-args]
;;    (loop [[effect continuation] (continuation coeffect-or-args)]
;;      (if (nil? continuation)
;;        effect
;;        (recur (continuation (exec-effect handlers effect))))))
;;   ([handlers continuation coeffect-or-args respond raise]
;;    (try
;;      (let [[effect continuation] (continuation coeffect-or-args)]
;;        (if (nil? continuation)
;;          (respond effect)
;;          (exec-effect handlers effect
;;                       (fn [coeffect]
;;                         (perform handlers continuation coeffect
;;                                  respond raise))
;;                       raise)))
;;      (catch #?(:clj Throwable, :cljs js/Error) error
;;        (raise error)))))
