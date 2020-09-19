(ns darkleaf.effect.proto
 (:refer-clojure :exclude [next]))

(defprotocol Continuation
  (done? [this])
  (effect [this])
  (next [this coeffect])
  (throw [this throwable])
  (return [this value])
  (clone [this]))
