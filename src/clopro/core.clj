(ns clopro.core
  (:require
    [clojure.zip :as z]
    [net.cgrand.enlive-html :as html]
    ))

(def main_url "https://en.wikipedia.org")
(def destination_uri "/wiki/Philosophy")

(def t_urls
  {
   :url_0  "/wiki/Index_(publishing)"
   :url_1  "/wiki/Biology"
   :url_2  "/wiki/Organism"
   :url_3  "/wiki/Special:Page"
   :url_4  "/wiki/Natural_science"
   :url_5  "/wiki/Empirical_evidence"
   :url_6  "/wiki/Wiktionary:Page"
   :url_7  "/wiki/Knowledge"
   :url_8  "/wiki/Discovery_(observation)"
   :url_9  "/wiki/Invention"
   :url_10 "/wiki/Autodesk_Inventor"
   :url_11 "/wiki/Outline_of_academic_disciplines"
   :url_12 "/wiki/Wiktionary:Page1"
   :url_13 "/wiki/Higher_education"
   :url_14 "/wiki/Europe"
   :url_15 "/wiki/Mediterranean_Sea"
   :url_16 "/wiki/Europe"
   :url_17 "/wiki/Iberian_Peninsula"
   :url_18 "/wiki/Spain"
   :url_19 "/wiki/Philosophy"
   })

(def ignore_urls
  ["/wiki/Special:(.*)"
   "/wiki/Wiktionary:(.*)"
   "/wiki/Help:(.*)"
   "/wiki/Portal:(.*)"
   "/wiki/Category:(.*)"
   "(.*)/File:(.*)"])

;Ignore urls
(defn bad-url?
  [input]
  (loop [remaining_urls ignore_urls]
    (if (empty? remaining_urls)
      false
      (let [[b_url & remaining] remaining_urls]
        (if (re-matches (re-pattern b_url) input)
          true
          (recur remaining)
          )))
    ))

(defn valid-wiki-url?
  [input]
  (clojure.string/starts-with? input "/wiki"))

(defn stripped
  [orig]
  (html/transform orig [(keyword "table")] (html/substitute "")))

(defn wiki-article-links
  [starting_url]
  (let [dom (html/html-resource (java.net.URL. starting_url))
        only-links (stripped dom)
        link-nodes (html/select only-links [:body :div#content :a])
        links (mapcat #(html/attr-values % :href) link-nodes)]

    (println links)
    (->> links
         (filter valid-wiki-url?)
         (filter (complement bad-url?)))
    ))


(defn start-scrape
  []
  (wiki-article-links main_url))