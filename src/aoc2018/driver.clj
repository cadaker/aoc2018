(ns aoc2018.driver)

(def solutions
  {})

(defmacro defsolution [solution-name args & body]
  `(alter-var-root #'solutions
                   assoc ~solution-name (fn ~args ~@body)))
