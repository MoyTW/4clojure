(ns scraper-4clojure.core
  (:require [clojure.string :as s])
  (:import (org.openqa.selenium By WebDriver WebElement)
           (org.openqa.selenium.htmlunit HtmlUnitDriver)))

(defn selenium-example []
  (let [browser (HtmlUnitDriver.)]
    (.get browser "http://www.google.com")
    (let [element (.findElement browser (By/name "q"))]
      (.sendKeys element (into-array ["Cheese!"]))
      (.submit element)
      (prn (.getTitle browser))
      (.quit browser))))

(defn login [username password browser]
  (.get browser "http://www.4clojure.com/login?")
      (let [un-field (.findElement browser (By/name "user"))
            pw-field (.findElement browser (By/name "pwd"))
            login (.findElement browser (By/xpath "//button[text()='Log In']"))]
        (.sendKeys un-field (into-array [username]))
        (.sendKeys pw-field (into-array [password]))
        (.click login)))

(defn parse-solution-page [s]
  (let [m (re-matcher #"(?s)\<pre.*\>(.*)\<\/pre\>" s)
        code (do (.find m) (clojure.string/trim (.group m 1)))]
    (-> code
        (s/replace #"&lt;" "<")
        (s/replace #"&gt;" ">")
        (s/replace #"&amp;" "&"))))

(defn process-title [title]
  (let [[number text] (s/split title #"\. " 2)]
  (str (format "%03d" (Integer/parseInt number))
       "-"
       (-> text 
           (s/replace #": " "-")
           (s/replace #">" "gt")
           (s/replace #"\s" "_")
           (s/replace #"'" "")))))

(defn scrape-problem [browser problem]
  (let [url (str "http://www.4clojure.com/problem/" problem)]
    (.get browser url)
    (when (= (.getCurrentUrl browser) url)
      (let [page-title (.getTitle browser)
            title (process-title page-title)]
        (.click (.findElement browser (By/id "solutions-link")))
        {:title title
         :content (str ";; 4Clojure Problem " page-title "\n"
                       ";; url: " url "\n"
                       (parse-solution-page (.getPageSource browser)))}))))

(defn get-and-write-problem [browser dir problem]
  (if-let [m (scrape-problem browser problem)]
    (let [outfile (str dir "/" (:title m) ".clj")]
      (prn (str "Writing " (:title m) " to " outfile))
      (spit outfile (:content m)))))

(defn scrape-4clojure [username password dir max]
  (let [browser (HtmlUnitDriver.)
        targets (range 1 (inc max))]
    (login username password browser)
    (doall (map #(get-and-write-problem browser dir %) targets))
    (.quit browser)))

(defn scrape-single [username password dir target]
  (let [browser (HtmlUnitDriver.)]
    (login username password browser)
    (get-and-write-problem browser dir target)
    (.quit browser)))