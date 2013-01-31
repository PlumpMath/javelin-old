(defproject tailrecursion/javelin "1.0.0-SNAPSHOT"
  :description "A Functional Reactive Programming library for ClojureScript"
  :url "https://github.com/tailrecursion/javelin"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[alandipert/cljs-priority-map "1.0.0-SNAPSHOT"]
                 [alandipert/desiderata "1.0.1"]]
  :plugins [[lein-cljsbuild "0.3.0"]]
  :source-paths ["src/clj"]
  :cljsbuild {:builds
              {:micha
               {:source-paths ["public" "src/cljs"]
                :compiler {:output-to "public/micha.js"
                           :optimizations :whitespace
                           :pretty-print true
                           :warnings true
                           ;; :optimizations :advanced
                           }
                :jar false}
               :alan
               {:source-paths ["public" "src/cljs"]
                :compiler {:output-to "public/alan.js"
                           ;; :optimizations :whitespace
                           ;; :pretty-print true
                           :warnings true
                           :optimizations :advanced
                           }
                :jar false}
               :test
               {:source-paths ["src/cljs" "test"]
                :compiler {:output-to "public/test.js"
                           :optimizations :advanced}
                :jar false}}
              :test-commands {"unit" ["phantomjs" "test/runner.js"]}})
