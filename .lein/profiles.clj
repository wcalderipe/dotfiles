{:user {:repl {:dependencies [;; Default Clojure version outside projects
                              ^:displace [org.clojure/clojure "1.10.0"]]}

        :dependencies [
                       [cider/cider-nrepl "0.20.0-SNAPSHOT"]
                       [lein-cljfmt "0.5.7"]
                       [mvxcvi/puget "1.0.2"]
                       [nrepl "0.5.3"]
                       [pjstadig/humane-test-output "0.8.3"]

                       ;; For Figwheel
                       [com.cemerick/piggieback "0.2.2"]
                       ]

        :plugins [
                  [com.jakemccrary/lein-test-refresh "0.22.0"]
                  [jonase/eastwood "0.2.6"]
                  [lein-ancient "0.6.15"]
                  [lein-exec "0.3.7"]
                  ]

        :repl-options {
                       ;; This expression will run when first opening a REPL, in the
                       ;; namespace from :init-ns or :main if specified.
                       :init (do (set! *print-length* 30)
                                 (set! *print-level* 20))

                       :nrepl-middleware [cider.nrepl/wrap-apropos
                                          cider.nrepl/wrap-classpath
                                          cider.nrepl/wrap-complete
                                          cider.nrepl/wrap-debug
                                          cider.nrepl/wrap-format
                                          cider.nrepl/wrap-info
                                          cider.nrepl/wrap-inspect
                                          cider.nrepl/wrap-macroexpand
                                          cider.nrepl/wrap-ns
                                          cider.nrepl/wrap-spec
                                          cider.nrepl/wrap-profile
                                          cider.nrepl/wrap-refresh
                                          cider.nrepl/wrap-resource
                                          cider.nrepl/wrap-stacktrace
                                          cider.nrepl/wrap-test
                                          cider.nrepl/wrap-trace
                                          cider.nrepl/wrap-out
                                          cider.nrepl/wrap-undef
                                          cider.nrepl/wrap-version]}

        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        }
 }
