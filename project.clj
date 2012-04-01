(defproject sandbar/sandbar "0.4.0-SNAPSHOT"
  :description "Clojure web application libraries built on top of Ring
                and Compojure."
  :url "http://github.com/brentonashworth/sandbar"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/data.json "0.1.2"]
                 [ring/ring-core "1.0.1"]
                 [compojure "1.0.1"]
                 [hiccup "0.3.7"]
                 [slingshot "0.10.1"]
                 [org.clojure/tools.macro "0.1.1"]
                 [inflections "0.6.6-SNAPSHOT"]
                 [org.clojure/tools.logging "0.2.3"]]
  :dev-dependencies [[jline "0.9.94"]
                     [ring/ring-devel "1.0.1"]
                     [ring/ring-jetty-adapter "1.0.1"]
                     [ring/ring-httpcore-adapter "0.3.5"]
                     [lein-difftest "1.3.7"]
                     [radagast "1.1.0"]
                     [enlive "1.0.0"]
                     [marginalia "0.7.0-SNAPSHOT"]]
  :hooks [leiningen.hooks.difftest]
  :radagast/ns-whitelist #"^sandbar.*")
