(defproject alandipert/waffle "1.0.0-SNAPSHOT"
  :description "A ClojureScript implementation of Flapjax"
  :url "https://github.com/alandipert/waffle"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[alandipert/cljs-priority-map "1.0.0-SNAPSHOT"]
                 [alandipert/desiderata "1.0.1"]]
  :plugins [[lein-cljsbuild "0.3.0"]]
  :source-paths ["src/clj"]
  :cljsbuild {:builds
              {:dev
               {:source-paths ["src/cljs"]
                :compiler {:output-to "public/dev.js"
                           :optimizations :whitespace
                           :pretty-print true
                           ;; :optimizations :advanced
                           }
                :jar false}
               :test
               {:source-paths ["src/cljs" "test"]
                :compiler {:output-to "public/test.js"
                           :optimizations :advanced}
                :jar false}}
              :test-commands {"unit" ["phantomjs" "test/runner.js"]}})
