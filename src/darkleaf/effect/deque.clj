(ns darkleaf.effect.deque
  (:refer-clojure :exclude [pop! peak])
  (:import
   [java.util Deque ArrayDeque Collection]))

(set! *warn-on-reflection* true)

(defn deque
  ([] (ArrayDeque.))
  ([^Collection coll] (ArrayDeque. coll)))

(defn add-first [^Deque deque value]
  (.addFirst deque value))

(defn poll-first [^Deque deque]
  (.pollFirst deque))

(defn peek-first [^Deque deque]
  (.peekFirst deque))
