;;;; This file is part of gorilla-repl. Copyright (C) 2014-, Jony Hudson.
;;;;
;;;; gorilla-repl is licenced to you under the MIT licence. See the file LICENCE.txt for full details.

(ns gorilla-repl.latex
  (:require [gorilla-renderable.core :as render]))

(defrecord Latex [content])

(defn latex [content] (Latex. content))

(extend-type Latex
  render/Renderable
  (render [self]
    {:type :latex :content (:content self) :value (pr-str self)}))