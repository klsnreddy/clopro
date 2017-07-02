(ns clopro.core
  (:require
    [clojure.zip :as z]
    [net.cgrand.enlive-html :as html]
    [clj-http.client :as client]
    ))

(def main_url "https://en.wikipedia.org")
(def destination_uri "/wiki/Philosophy")

(def ignore_urls
  ["/wiki/Special:(.*)"
   "/wiki/Wiktionary:(.*)"
   "/wiki/Help:(.*)"
   "/wiki/Portal:(.*)"
   "/wiki/Category:(.*)"
   "(.*)/File:(.*)"
   "/wiki/Wikipedia:Protection_policy(.*)"
   "/wiki/Wikipedia:(.*)"
   "/wiki/User:(.*)"
   "(.*)(disambiguation)(.*)"
   ])

(defn complete-url
  [uri]
  (str main_url uri))

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
          )))))

(defn valid-wiki-url?
  [input]
  (clojure.string/starts-with? input "/wiki"))

(defn strip-dom
  [dom]
  (let [no-table (html/transform dom [:table] (html/substitute ""))
        no-i (html/transform no-table [:i] (html/substitute ""))
        no-not-searchable (html/transform no-table [:div.navigation-not-searchable] (html/substitute ""))]
    no-not-searchable))

(defn html-with-no-parentheses
  [starting_url]
  (let [req (client/get starting_url {:cookie-policy :standard})
        body (:body req)
        req-no-parentheses (clojure.string/replace body #"\\(.+?\\)" "")
        dom-tree (html/html-snippet req-no-parentheses)]
    dom-tree))

(defn non-visited-uri
  [visited_uris current_links]
  (loop [current_uris current_links]
    (let [[f_uri & remaining] current_uris]
      (if ((complement contains?) visited_uris f_uri)
        f_uri
        (recur remaining)))))

(defn wiki-first-link
  [starting_url visited_uris]
  (let [dom (html-with-no-parentheses starting_url)
        only-links (strip-dom dom)
        link-nodes (html/select only-links [:body :div#bodyContent :a])
        links (mapcat #(html/attr-values % :href) link-nodes)]
    ;(print visited_uris)
    (->> links
         (filter valid-wiki-url?)
         (filter (complement bad-url?))
         (non-visited-uri visited_uris))))


(defn scrape-wiki
  [main_uri]
  (loop [uri main_uri
         visited_uris [main_uri]]
    (if (= uri destination_uri)
      (println visited_uris)
      (let [next_uri (wiki-first-link (complete-url uri) visited_uris)
            next_url (complete-url next_uri)]
        (println next_url)
        (recur next_uri (conj visited_uris next_uri))))))

(defn start-scrape
  [input_uri]
  (scrape-wiki input_uri))
