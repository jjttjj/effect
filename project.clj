(defproject darkleaf/effect "0.0.1"
  :description "Effect system"
  :url "https://github.com/darkleaf/effect/"
  :license {:name "Unlicense"
            :url  "http://unlicense.org/"}
  :dependencies [[org.clojure/clojure "1.10.1" :scope "provided"]
                 [org.clojure/clojurescript "1.10.520" :scope "provided"]
                 [cloroutine "8"]]
  :plugins [[lein-doo "0.1.11"]]

  :cljsbuild
  {:builds [{:id           "node-none"
             :source-paths ["src" "test"]
             :compiler     {:output-to     "out/node-none.js"
                            :target        :nodejs
                            :optimizations :none
                            :main          darkleaf.effect.runner}}
            {:id           "node-advanced"
             :source-paths ["src" "test"]
             :compiler     {:output-to     "out/node-advanced.js"
                            :target        :nodejs
                            :optimizations :advanced
                            :main          darkleaf.effect.runner}}]}

  :repl-options {:init-ns darkleaf.effect.core})