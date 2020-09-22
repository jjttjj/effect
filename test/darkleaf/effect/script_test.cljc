(ns darkleaf.effect.script-test
  (:require
   [darkleaf.effect.script :as script]
   [darkleaf.effect.core :refer [! effect with-effects]]
   [clojure.test :as t])
  (:import
   #?(:clj [clojure.lang ExceptionInfo])))

(t/deftest args-test
  (let [ef     (fn [arg-1 arg-2]
                 (with-effects
                   (+ arg-1 arg-2)))
        script [{:args [1 2]}
                {:value 3}]]
    (script/test ef script)))

(t/deftest value-test
  (let [ef     (fn []
                 (with-effects
                   42))
        script [{:args []}
                {:value 42}]]
    (script/test ef script)))

(t/deftest wrong-value-test
  (let [ef     (fn []
                 (with-effects
                   :wrong))
        script [{:args []}
                {:value 42}]]
    (t/is (= {:type     :fail
              :expected 42
              :actual   :wrong
              :diffs [[:wrong
                       [42 :wrong nil]]]
              :message  "A wrong value"}
             (script/test* ef script)))))

(t/deftest extra-effect-test
  (let [ef     (fn []
                 (with-effects
                   (! (effect :my-effect))))
        script [{:args []}
                ::whatever]]
    (t/is (= {:type     :fail
              :expected nil
              :actual   (effect :my-effect)
              :message  "An extra effect"}
             (script/test* ef script)))))

(t/deftest effect-coeffect-test
  (let [ef     (fn []
                 (with-effects
                   (! (effect :my-effect :arg-1))))
        script [{:args []}
                {:effect   (effect :my-effect :arg-1)
                 :coeffect 42}
                {:value 42}]]
    (script/test ef script)))

(t/deftest wrong-effect-test
  (let [ef     (fn []
                 (with-effects
                   (! (effect :my-wrong-effect :arg-1))))
        script [{:args []}
                {:effect   (effect :my-effect :arg-1)
                 :coeffect 42}
                ::whatever]]
    (t/is (= {:type     :fail
              :expected (effect :my-effect :arg-1)
              :actual   (effect :my-wrong-effect :arg-1)
              :diffs    [[(effect :my-wrong-effect :arg-1)
                          [{:tag :my-effect}
                           {:tag :my-wrong-effect}
                           {:args [:arg-1]}]]]
              :message  "A wrong effect"}
             (script/test* ef script)))))

(t/deftest unfinished-continuation-test
  (let [ef     (fn []
                 (with-effects
                   42))
        script [{:args []}
                {:effect   (effect :my-effect)
                 :coeffect :my-value}
                ::whatever]]
    (t/is (= {:type     :fail
              :expected (effect :my-effect)
              :actual   42
              :message  "An unfinished continuation. An effect is expected."}
             (script/test* ef script)))))

(t/deftest return-test
  (let [ef     (fn []
                 (with-effects
                   (! (effect :my-effect))
                   42))
        script [{:args []}
                {:effect (effect :my-effect)
                 :return 0}
                {:value 0}]]
    (script/test ef script)))

(t/deftest return-with-finally-test
  (let [ef     (fn []
                 (with-effects
                   (try
                     (! (effect :my-effect))
                     (finally
                       (! (effect :finish))))
                   42))
        script [{:args []}
                {:effect (effect :my-effect)
                 :return 0}
                {:effect   (effect :finish)
                 :coeffect nil}
                {:value 0}]]
    (script/test ef script)))

(t/deftest throw-test
  (let [ef     (fn []
                 (with-effects
                   (try
                     (! (effect :my-effect))
                     (catch Exception ex
                       (ex-message ex)))))
        script [{:args []}
                {:effect (effect :my-effect)
                 :throw  (ex-info "Error" {})}
                {:value "Error"}]]
    (script/test ef script)))

(t/deftest thrown-test
  (let [ef     (fn []
                 (with-effects
                   (! (effect :my-effect))))
        script [{:args []}
                {:effect (effect :my-effect)
                 :throw  (ex-info "Error" {})}
                ::whatever]]
    (t/is (thrown-with-msg? ExceptionInfo #"Error"
                            (script/test ef script)))))

(t/deftest stack-use-case
  (let [nested-ef (fn [x]
                    (with-effects
                      (! (effect :prn "start nested-ef"))
                      (! (effect :prn x))
                      (! (effect :read))))
        ef        (fn [x]
                    (with-effects
                      (! (effect :prn "start ef"))
                      (! (nested-ef x))))
        script    [{:args ["some val"]}
                   {:effect   (effect :prn "start ef")
                    :coeffect nil}
                   {:effect   (effect :prn "start nested-ef")
                    :coeffect nil}
                   {:effect   (effect :prn "some val")
                    :coeffect nil}
                   {:effect   (effect :read)
                    :coeffect "input string"}
                   {:value "input string"}]]
    (script/test ef script)))

(t/deftest stacked-throw-test
  (let [nested-ef (fn []
                    (with-effects
                      (! (effect :my-effect))))
        ef     (fn []
                 (with-effects
                   (try
                     (! (nested-ef))
                     (catch ExceptionInfo ex
                       (ex-message ex)))))
        script [{:args []}
                {:effect (effect :my-effect)
                 :throw  (ex-info "Error" {})}
                {:value "Error"}]]
    (script/test ef script)))

(t/deftest script)
    ;; (t/testing "exception as coeffect"
    ;;   (let [script [{:args [:value]}
    ;;                 {:effect   [:some-eff :value]
    ;;                  :coeffect (ex-info "Fail" {})}
    ;;                 {:thrown {:type    ExceptionInfo
    ;;                           :message "Fail"
    ;;                           :data    {}}}]]
    ;;     (script/test continuation script)))
    ;; (t/testing "final-effect"
    ;;   (let [script [{:args [:value]}
    ;;                 {:final-effect [:some-eff :value]}]]
    ;;     (script/test continuation script)))
    ;; (t/testing "wrong final-effect"
    ;;   (let [script [{:args [:value]}
    ;;                 {:final-effect [:wrong]}]]
    ;;     (t/is (= {:type     :fail
    ;;               :expected [:wrong]
    ;;               :actual   [:some-eff :value]
    ;;               :diffs    [[[:some-eff :value]
    ;;                           [[:wrong] [:some-eff :value] nil]]]
    ;;               :message  "Wrong final effect"}
    ;;              (script/test* continuation script)))))
    ;; (t/testing "final-effect instead return"
    ;;   (let [script [{:args [:value]}
    ;;                 {:effect   [:some-eff :value]
    ;;                  :coeffect :other-value}
    ;;                 {:final-effect :other-value}]]
    ;;     (t/is (= {:type     :fail
    ;;               :expected '(some? continuation)
    ;;               :actual   :other-value
    ;;               :message  "A value was unexpectedly returned"}
    ;;              (script/test* continuation script)))))


#_(t/deftest exception
    (let [ef           (fn []
                         (with-effects
                           (! (effect :some-eff))
                           (throw (ex-info "Message" {:foo :bar}))))
          continuation (e/continuation ef)]
      (t/testing "correct"
        (let [script [{:args []}
                      {:effect   [:some-eff]
                       :coeffect :some-coeff}
                      {:thrown {:type    ExceptionInfo
                                :message "Message"
                                :data    {:foo :bar}}}]]
          (script/test continuation script)))
      (t/testing "unexpected exception"
        (let [script [{:args []}
                      {:effect   [:some-eff]
                       :coeffect :some-coeff}
                      {:return :ok}]
              report (script/test* continuation script)]
          (t/is (= :error (:type report)))))
      (t/testing "wrong exception type"
        (let [wrong-ex #?(:clj RuntimeException :cljs js/Error)
              script   [{:args []}
                        {:effect   [:some-eff]
                         :coeffect :some-coeff}
                        {:thrown {:type    wrong-ex
                                  :message "Some msg"
                                  :data    nil}}]
              report   (script/test* continuation script)]
          (t/is (= {:type     :fail
                    :expected {:type    wrong-ex
                               :message "Some msg"
                               :data    nil}
                    :actual   {:type    ExceptionInfo
                               :message "Message"
                               :data    {:foo :bar}}
                    :diffs    [[{:type    ExceptionInfo
                                 :message "Message"
                                 :data    {:foo :bar}}
                                [{:type    wrong-ex
                                  :message "Some msg"
                                  :data    nil}
                                 {:type    ExceptionInfo
                                  :message "Message"
                                  :data    {:foo :bar}}

                                 nil]]]
                    :message "Wrong exception"}
                   report))))
      (t/testing "wrong exception message"
        (let [script [{:args []}
                      {:effect   [:some-eff]
                       :coeffect :some-coeff}
                      {:thrown {:type    ExceptionInfo
                                :message "Wrong message"
                                :data    {:foo :bar}}}]
              report (script/test* continuation script)]
          (t/is (=  {:type     :fail
                     :expected {:type    ExceptionInfo
                                :message "Wrong message"
                                :data    {:foo :bar}}
                     :actual   {:type    ExceptionInfo
                                :message "Message"
                                :data    {:foo :bar}}
                     :diffs    [[{:type    ExceptionInfo
                                  :message "Message"
                                  :data    {:foo :bar}}
                                 [{:message "Wrong message"}
                                  {:message "Message"}
                                  {:type ExceptionInfo
                                   :data {:foo :bar}}]]]
                     :message  "Wrong exception"}
                    report))))
      (t/testing "wrong exception data"
        (let [script [{:args []}
                      {:effect   [:some-eff]
                       :coeffect :some-coeff}
                      {:thrown {:type    ExceptionInfo
                                :message "Message"
                                :data    {:foo :wrong}}}]
              report (script/test* continuation script)]
          (t/is (= {:type     :fail
                    :expected {:type    ExceptionInfo
                               :message "Message"
                               :data    {:foo :wrong}}
                    :actual   {:type    ExceptionInfo
                               :message "Message"
                               :data    {:foo :bar}}
                    :diffs    [[{:type    ExceptionInfo
                                 :message "Message"
                                 :data    {:foo :bar}}
                                [{:data {:foo :wrong}}
                                 {:data {:foo :bar}}
                                 {:type    ExceptionInfo
                                  :message "Message"}]]]
                    :message  "Wrong exception"}
                   report))))))










;; (t/deftest script
;;   (let [ef           (fn [x]
;;                        (with-effects
;;                          (! (effect :some-eff x))))
;;         continuation (e/continuation ef)]
;;     (t/testing "correct"
;;       (let [script [{:args [:value]}
;;                     {:effect   [:some-eff :value]
;;                      :coeffect :other-value}
;;                     {:return :other-value}]]
;;         (script/test continuation script)))
;;     (t/testing "exception as coeffect"
;;       (let [script [{:args [:value]}
;;                     {:effect   [:some-eff :value]
;;                      :coeffect (ex-info "Fail" {})}
;;                     {:thrown {:type    ExceptionInfo
;;                               :message "Fail"
;;                               :data    {}}}]]
;;         (script/test continuation script)))
;;     (t/testing "final-effect"
;;       (let [script [{:args [:value]}
;;                     {:final-effect [:some-eff :value]}]]
;;         (script/test continuation script)))
;;     (t/testing "wrong effect"
;;       (let [script [{:args [:value]}
;;                     {:effect   [:wrong]
;;                      :coeffect :other-value}
;;                     {:return :other-value}]]
;;         (t/is (= {:type     :fail
;;                   :expected [:wrong]
;;                   :actual   [:some-eff :value]
;;                   :diffs    [[[:some-eff :value]
;;                               [[:wrong] [:some-eff :value] nil]]],
;;                   :message  "Wrong effect"}
;;                  (script/test* continuation script)))))
;;     (t/testing "wrong return"
;;       (let [script [{:args [:value]}
;;                     {:effect   [:some-eff :value]
;;                      :coeffect :other-value}
;;                     {:return :wrong}]]
;;         (t/is (= {:type     :fail
;;                   :expected :wrong
;;                   :actual   :other-value
;;                   :diffs    [[:other-value
;;                               [:wrong :other-value nil]]]
;;                   :message  "Wrong return"}
;;                  (script/test* continuation script)))))
;;     (t/testing "wrong final-effect"
;;       (let [script [{:args [:value]}
;;                     {:final-effect [:wrong]}]]
;;         (t/is (= {:type     :fail
;;                   :expected [:wrong]
;;                   :actual   [:some-eff :value]
;;                   :diffs    [[[:some-eff :value]
;;                               [[:wrong] [:some-eff :value] nil]]]
;;                   :message  "Wrong final effect"}
;;                  (script/test* continuation script)))))
;;     (t/testing "extra effect"
;;       (let [script [{:args [:value]}
;;                     {:return :wrong}]]
;;         (t/is (=  {:type     :fail
;;                    :expected nil
;;                    :actual   [:some-eff :value]
;;                    :message  "Extra effect"}
;;                   (script/test* continuation script)))))
;;     (t/testing "missed effect"
;;       (let [script [{:args [:value]}
;;                     {:effect   [:some-eff :value]
;;                      :coeffect :other-value}
;;                     {:effect   [:extra-eff :value]
;;                      :coeffect :some-value}
;;                     {:return :some-other-value}]]
;;         (t/is (= {:type     :fail
;;                   :expected [:extra-eff :value]
;;                   :actual   :other-value
;;                   :message  "Unexpected return. An effect is expected."}
;;                  (script/test* continuation script)))))
;;     (t/testing "final-effect instead return"
;;       (let [script [{:args [:value]}
;;                     {:effect   [:some-eff :value]
;;                      :coeffect :other-value}
;;                     {:final-effect :other-value}]]
;;         (t/is (= {:type     :fail
;;                   :expected '(some? continuation)
;;                   :actual   :other-value
;;                   :message  "A value was unexpectedly returned"}
;;                  (script/test* continuation script)))))))

;; (t/deftest trivial-script
;;   (let [ef           (fn [x]
;;                        (with-effects
;;                          x))
;;         continuation (e/continuation ef)
;;         script       [{:args [:value]}
;;                       {:return :value}]]
;;     (script/test continuation script)))

;; (t/deftest exception
;;   (let [ef           (fn []
;;                        (with-effects
;;                          (! (effect :some-eff))
;;                          (throw (ex-info "Message" {:foo :bar}))))
;;         continuation (e/continuation ef)]
;;     (t/testing "correct"
;;       (let [script [{:args []}
;;                     {:effect   [:some-eff]
;;                      :coeffect :some-coeff}
;;                     {:thrown {:type    ExceptionInfo
;;                               :message "Message"
;;                               :data    {:foo :bar}}}]]
;;         (script/test continuation script)))
;;     (t/testing "unexpected exception"
;;       (let [script [{:args []}
;;                     {:effect   [:some-eff]
;;                      :coeffect :some-coeff}
;;                     {:return :ok}]
;;             report (script/test* continuation script)]
;;         (t/is (= :error (:type report)))))
;;     (t/testing "wrong exception type"
;;       (let [wrong-ex #?(:clj RuntimeException :cljs js/Error)
;;             script   [{:args []}
;;                       {:effect   [:some-eff]
;;                        :coeffect :some-coeff}
;;                       {:thrown {:type    wrong-ex
;;                                 :message "Some msg"
;;                                 :data    nil}}]
;;             report   (script/test* continuation script)]
;;         (t/is (= {:type     :fail
;;                   :expected {:type    wrong-ex
;;                              :message "Some msg"
;;                              :data    nil}
;;                   :actual   {:type    ExceptionInfo
;;                              :message "Message"
;;                              :data    {:foo :bar}}
;;                   :diffs    [[{:type    ExceptionInfo
;;                                :message "Message"
;;                                :data    {:foo :bar}}
;;                               [{:type    wrong-ex
;;                                 :message "Some msg"
;;                                 :data    nil}
;;                                {:type    ExceptionInfo
;;                                 :message "Message"
;;                                 :data    {:foo :bar}}

;;                                nil]]]
;;                   :message "Wrong exception"}
;;                  report))))
;;     (t/testing "wrong exception message"
;;       (let [script [{:args []}
;;                     {:effect   [:some-eff]
;;                      :coeffect :some-coeff}
;;                     {:thrown {:type    ExceptionInfo
;;                               :message "Wrong message"
;;                               :data    {:foo :bar}}}]
;;             report (script/test* continuation script)]
;;         (t/is (=  {:type     :fail
;;                    :expected {:type    ExceptionInfo
;;                               :message "Wrong message"
;;                               :data    {:foo :bar}}
;;                    :actual   {:type    ExceptionInfo
;;                               :message "Message"
;;                               :data    {:foo :bar}}
;;                    :diffs    [[{:type    ExceptionInfo
;;                                 :message "Message"
;;                                 :data    {:foo :bar}}
;;                                [{:message "Wrong message"}
;;                                 {:message "Message"}
;;                                 {:type ExceptionInfo
;;                                  :data {:foo :bar}}]]]
;;                    :message  "Wrong exception"}
;;                   report))))
;;     (t/testing "wrong exception data"
;;       (let [script [{:args []}
;;                     {:effect   [:some-eff]
;;                      :coeffect :some-coeff}
;;                     {:thrown {:type    ExceptionInfo
;;                               :message "Message"
;;                               :data    {:foo :wrong}}}]
;;             report (script/test* continuation script)]
;;         (t/is (= {:type     :fail
;;                   :expected {:type    ExceptionInfo
;;                              :message "Message"
;;                              :data    {:foo :wrong}}
;;                   :actual   {:type    ExceptionInfo
;;                              :message "Message"
;;                              :data    {:foo :bar}}
;;                   :diffs    [[{:type    ExceptionInfo
;;                                :message "Message"
;;                                :data    {:foo :bar}}
;;                               [{:data {:foo :wrong}}
;;                                {:data {:foo :bar}}
;;                                {:type    ExceptionInfo
;;                                 :message "Message"}]]]
;;                   :message  "Wrong exception"}
;;                  report))))))
