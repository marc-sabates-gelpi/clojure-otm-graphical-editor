(defproject otm-graphical-editor "0.1.0"
  :description "OTM Graphical Editor"
  :license {:name "GNU General Public License (GPL) version 3"
            :url "https://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :main ^:skip-aot otm-graphical-editor.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
