(ns darkleaf.effect.middleware.reduced-test
  (:require
   [darkleaf.effect.core :as e :refer [with-effects ! effect]]
   [darkleaf.effect.script :as script]
   [darkleaf.effect.middleware.reduced :as reduced]
   [clojure.test :as t]))

(t/deftest maybe-example
  (let [ef       (fn [x]
                   (with-effects
                     (+ 5 (! (effect :maybe x)))))
        handlers {:maybe (fn [value]
                           (if (nil? value)
                             (reduced nil)
                             value))}]
    (t/testing "interpretator"
      (let [continuation (-> ef
                             (e/continuation)
                             (reduced/wrap-reduced))]
        (t/is (= 6 (e/perform handlers continuation [1])))
        (t/is (= nil (e/perform handlers continuation [nil])))))
    (t/testing "script"
      (let [continuation (e/continuation ef)]
        (t/testing :just
          (let [script [{:args [1]}
                        {:effect   [:maybe 1]
                         :coeffect 1}
                        {:return 6}]]
            (script/test continuation script)))
        (t/testing :nothing
          (let [script [{:args [nil]}
                        {:final-effect [:maybe nil]}]]
            (script/test continuation script)))))
    (t/testing "script with applied middleware"
      (let [continuation (-> ef
                             (e/continuation)
                             (reduced/wrap-reduced))]
        (t/testing :just
          (let [script [{:args [1]}
                        {:effect   [:maybe 1]
                         :coeffect 1}
                        {:return 6}]]
            (script/test continuation script)))
        (t/testing :nothing
          (let [script [{:args [nil]}
                        {:effect   [:maybe nil]
                         :coeffect (reduced nil)}
                        {:return nil}]]
            (script/test continuation script)))))))
