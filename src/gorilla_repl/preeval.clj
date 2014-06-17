(ns gorilla-repl.preeval
  "Code for evaluating the worksheet code when the browser
  loads it.

  The main idea is to allow the user to opt out of the tedium
  of evaling setup code in the worksheet. The mechanism is
  metadata, of two sorts. The logic is as follows:

    - By default, eval any form that has :eval metadata
    - If the first form is an ns form and it has :eval-all
      metadata, then eval all subsequent forms unless they
      have :no-eval metadata
    - Support metadata both on the top-level form, and on
      the second element of a top level list if it is a
      symbol")

(defn ^:private flex-meta
  [form]
  (merge (meta form)
         (or
          (and (seq? form)
               (symbol? (second form))
               (meta (second form)))
          {})))

(defn ^:private read-forms
  [s]
  (let [sentinel (Object.)
        r (-> s
              java.io.StringReader.
              java.io.PushbackReader.)]
    (loop [forms []]
      (let [x (read r false sentinel)]
        (if (= sentinel x) forms (recur (conj forms x)))))))

(defn preeval
  [worksheet-string]
  (let [forms (read-forms worksheet-string)
        maybe-ns-form (first forms)
        ns-meta (if (= 'ns (and (seq? maybe-ns-form) (first maybe-ns-form)))
                  (flex-meta maybe-ns-form))
        blacklist? (:eval-all ns-meta)]
    (binding [*ns* (the-ns 'user)]
      (doseq [form forms
              :let [form-meta (flex-meta form)]
              :when (if blacklist?
                      (not (:no-eval form-meta))
                      (:eval form-meta))]
        (eval form)))))
