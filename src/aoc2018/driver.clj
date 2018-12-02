(ns aoc2018.driver)

(def solutions
  {})

;; TODO: arguments
(defmacro defsolution [solution-name args & body]
  `(alter-var-root #'solutions
                   assoc ~solution-name (fn ~args ~@body)))
