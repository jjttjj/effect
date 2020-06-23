(ns darkleaf.effect.middleware.context
  (:require [darkleaf.effect.internal :as i]))

(defn wrap-context [continuation])
;;   (let [fallback-context (volatile! nil)]
;;     (fn [raw-coeffect]
;;       (let [[context coeffect] (if (i/throwable? raw-coeffect)
;;                                  [@fallback-context raw-coeffect]
;;                                  raw-coeffect)
;;             effect             (continuation coeffect)]
;;         (if (not= :effect (i/kind effect))
;;           [context effect]
;;           [effect])))))


;; ;; тут не хватает isDone
