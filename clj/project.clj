(defproject powermail "0.1.0-SNAPSHOT" :description "Email assistant"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.bmillare/dj.peg "0.3.0"]
				 [clj-time "0.6.0"]
				 [javax.mail/mail "1.4"]]
  :main powermail.core)
