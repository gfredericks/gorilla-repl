;;;; This file is part of gorilla-repl. Copyright (C) 2014-, Jony Hudson and contributors.
;;;;
;;;; gorilla-repl is licenced to you under the MIT licence. See the file LICENCE.txt for full details.

(ns gorilla-repl.worksheet-export
  (:require [cheshire.core :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [instaparse.core :as insta]
            [hiccup.core :refer :all]
            [markdown.core :as md]))

(defn uncomment
  [xs]
  (string/join
    #"\n"
    (map
      (fn [x]
        (subs x 4))
      (string/split-lines xs))))

(def gorilla-worksheet
  (insta/parser
    "worksheet = worksheetHeader segmentWithBlankLine*

     lineEnd = '\\n' / '\\r\\n'

     worksheetHeader = ';; gorilla-repl.fileformat = 1' lineEnd lineEnd

     segmentWithBlankLine = segment lineEnd?

     segment = freeSegment / codeSegment

     freeSegment = freeSegmentOpenTag stringNoDelim? freeSegmentCloseTag

     freeSegmentOpenTag = ';; **' lineEnd

     freeSegmentCloseTag = lineEnd ';; **' lineEnd

     codeSegment = codeSegmentOpenTag stringNoDelim? codeSegmentCloseTag consoleSection? outputSection?

     codeSegmentOpenTag = ';; @@' lineEnd

     codeSegmentCloseTag = lineEnd ';; @@' lineEnd

     outputSection = outputOpenTag stringNoDelim outputCloseTag

     outputOpenTag = ';; =>' lineEnd

     outputCloseTag = lineEnd ';; <=' lineEnd

     consoleSection = consoleOpenTag stringNoDelim consoleCloseTag

     consoleOpenTag = ';; ->' lineEnd

     consoleCloseTag = lineEnd ';; <-' lineEnd

     stringNoDelim = noDelimChar+

     delimiter = freeSegmentOpenTag / freeSegmentCloseTag / codeSegmentOpenTag / codeSegmentCloseTag / outputOpenTag / outputCloseTag / consoleOpenTag / consoleCloseTag

     noDelimChar = !delimiter #'.|\\s'"))

(defn remove-open-close-tags
  [segment open-tag close-tag]
  (filter
    #(and
      (not= (first %) open-tag)
      (not= (first %) close-tag))
    segment))

(defn format-code
  [code]
  (str "<pre>" code "</pre>"))

(defn render-free-segment
  [a-free-segment]
  (html [:div {:class "segment free"} (md/md-to-html-string a-free-segment)]))

(defn render-clojure-code
  [a-code-segment]
  (html [:div {:class "segment-main"}
         [:pre {:data-lang "clojure"}
          [:code.clojure a-code-segment]]]))

(defn render-worksheet
  [segments]
  (html [:html
         [:head
          [:link {:rel  "stylesheet"
                  :href "http://fonts.googleapis.com/css?family=Arvo:400,700,400italic,700italic|Lora:400,700,400italic,700italic"
                  :type "text/css"}]
          [:style (slurp (io/resource "public/css/worksheet.css"))]
          [:style (slurp (io/resource "public/css/output.css"))]
          [:link {:rel "stylesheet"
                  :href "http://cdnjs.cloudflare.com/ajax/libs/codemirror/3.20.0/codemirror.css"}]
          [:script {:type "text/javascript"
                    :src  "http://code.jquery.com/jquery-1.11.0.min.js"}]
          [:script {:type "text/javascript"
                    :src "http://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.5.2/underscore-min.js"}]
          [:script {:type "text/javascript"
                    :src  "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_SVG-full.js&amp;delayStartupUntil=configured"}]
          [:script {:type "text/javascript"
                    :src "http://cdnjs.cloudflare.com/ajax/libs/codemirror/3.20.0/addon/runmode/runmode-standalone.js"}]
          [:script {:type "text/javascript"
                    :src "http://cdnjs.cloudflare.com/ajax/libs/codemirror/3.20.0/addon/runmode/colorize.js"}]
          [:script {:type "text/javascript"
                    :src  "http://cdnjs.cloudflare.com/ajax/libs/codemirror/3.20.0/mode/clojure/clojure.js"}]
          [:script {:type "text/javascript"
                    :src  "http://d3js.org/d3.v3.min.js"}]
          [:script {:type "text/javascript"
                    :src  "http://cdnjs.cloudflare.com/ajax/libs/d3-geo-projection/0.2.9/d3.geo.projection.min.js"}]
          [:script {:type "text/javascript"
                    :src  "http://cdnjs.cloudflare.com/ajax/libs/vega/1.3.3/vega.min.js"}]
          [:script {:type "text/javascript"} (slurp (io/resource "public/jslib/uuid/uuid.core.js"))]
          [:script {:type "text/javascript"} (slurp (io/resource "public/js/renderer.js"))]
          [:script {:type "text/javascript"} (slurp (io/resource "gorilla_repl/standalone.js"))]]
         [:body
          [:div#contents segments]]]))

(defn worksheet-str->standalone-html
  [worksheet]
  (->> (gorilla-worksheet worksheet)
       (insta/transform
         {:worksheet            (fn [& xs] (render-worksheet (rest xs)))
          :segmentWithBlankLine (fn [& xs] (first xs))
          :segment              (fn [& xs] (first xs))
          :freeSegment          (fn [& xs]
                                  (render-free-segment
                                    (uncomment
                                      (first
                                        (remove-open-close-tags xs
                                                                :freeSegmentOpenTag
                                                                :freeSegmentCloseTag)))))
          :codeSegment          (fn [& xs]
                                  (let [code-segment
                                        (remove-open-close-tags xs
                                                                :codeSegmentOpenTag
                                                                :codeSegmentCloseTag)]
                                    (html
                                      [:div {:class "segment code"}
                                       (if-not (empty? code-segment)
                                         (cons
                                           (render-clojure-code
                                             (first code-segment))
                                           (rest code-segment))
                                         code-segment)])))
          :consoleSection       (fn [& xs]
                                  (html
                                    [:div.console-text
                                     (uncomment
                                       (first
                                         (remove-open-close-tags xs
                                                                 :consoleOpenTag
                                                                 :consoleCloseTag)))]))
          :outputSection        (fn [& xs]
                                  (html
                                    [:div.output
                                     [:div]
                                     [:script
                                      {:type "text/javascript"}
                                      "
         var eles = document.getElementsByTagName('script');
         ele = eles[eles.length - 1].parentNode.firstChild;"
                                      (str
                                        "render(JSON.parse("
                                        (encode
                                          (uncomment
                                            (first
                                              (remove-open-close-tags xs
                                                                      :outputOpenTag
                                                                      :outputCloseTag))))
                                        "), ele)")]]))
          :stringNoDelim        (fn [& xs]
                                  (apply str (map second xs)))})))

(defn worksheet->standalone-html
  [filename]
  (worksheet-str->standalone-html
    (slurp filename)))


