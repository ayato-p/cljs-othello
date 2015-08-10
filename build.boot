(set-env!
 :source-paths #{"src"}
 :resource-paths #{"resources"}
 :dependencies '[[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.48"]
                 [org.clojure/core.match "0.3.0-alpha4"]

                 [reagent "0.5.0"]
                 [re-frame "0.4.1"]

                 [adzerk/boot-cljs "1.0-SNAPSHOT" :scope "test"]
                 [adzerk/boot-reload "0.3.1" :scope "test"]
                 [adzerk/boot-cljs-repl "0.1.10-SNAPSHOT" :scope "test"]
                 [pandeiro/boot-http "0.6.3-SNAPSHOT" :scope "test"]])

(require '[adzerk.boot-reload :refer :all]
         '[adzerk.boot-cljs :refer :all]
         '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
         '[pandeiro.boot-http :refer [serve]])

(deftask dev []
  (comp (serve :dir "target/")
        (repl :server true)
        (watch)
        (reload :on-jsload 'othello.core/main)
        (cljs-repl)
        (cljs :source-map true :optimizations :none)))

(deftask build []
  (comp (cljs :optimizations :advanced)))
