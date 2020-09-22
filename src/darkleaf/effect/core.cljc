(ns darkleaf.effect.core
  (:require
   [darkleaf.effect.impl :as i]
   [darkleaf.effect.proto :as p]))
  ;;  [darkleaf.effect.internal :as i])
  ;; #?(:cljs (:require-macros [darkleaf.effect.core :refer [with-effects]])))

(defn effect [tag & args]
  (i/->Effect tag args))

(defn ! [x]
  x)

(defmacro with-effects [& body]
  (i/body->continuation `! body))

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

(defn perform
  ([handlers continuation]
   (let [continuation (i/wrapper continuation)]
     (try
       (while (not (p/done? continuation))
         (let [{:keys [tag args]} (p/effect continuation)
               handler            (handlers tag)
               coeffect           (apply handler args)]
           (p/next continuation coeffect)))
       (p/effect continuation)
       (catch Exception ex
         (throw (ex-info "Error performing continuation"
                         {:effect (p/effect continuation)
                          :done?  (p/done? continuation)}
                         ex))))))

  #_([handlers continuation coeffect-or-args respond raise]
     (try
       (let [[effect continuation] (continuation coeffect-or-args)]
         (if (nil? continuation)
           (respond effect)
           (exec-effect handlers effect
                        (fn [coeffect]
                          (perform handlers continuation coeffect
                                   respond raise))
                        raise)))
       (catch #?(:clj Throwable, :cljs js/Error) error
         (raise error)))))
