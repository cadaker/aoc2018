(ns aoc2018.driver)

(def solutions
  {})

(defmacro defsolution [solution-name args & body]
  `(alter-var-root #'solutions
                   assoc (name '~solution-name) (fn ~args ~@body)))
