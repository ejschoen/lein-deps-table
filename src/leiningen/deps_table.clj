(ns leiningen.deps-table
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.csv :as csv])
  (:require [leiningen.core.classpath :as classpath]
            [leiningen.core.project :as project]
            ))

(def deps-command
  {":dependencies" [:dependencies :managed-dependencies]
   ":managed-dependencies" [:managed-dependencies nil]})
  
(defn flatten-deps
  [hierarchy]
  (let [rows (loop [deps []
                    queue [hierarchy]]
               (if (empty? queue)
                 deps
                 (let [dep (first queue)
                       direct-deps (keys dep)
                       indirect-deps (filter identity (vals dep))]
                   (recur (concat deps direct-deps)
                          (concat (rest queue) indirect-deps)))))]
    (sort-by first rows)))

(defn rowify
  [row]
  (let [rowmap (apply hash-map (drop 2 row))]
    [(first row)
     (second row)
     (if (:exclusions rowmap)
       (str/join ",\n" (map (fn [x] (if (or (seq? x) (vector? x) (list? x))
                                    (first x)
                                    x))
                          (:exclusions rowmap))))
     (if (:classifier rowmap)
       (:classifier rowmap))
     ]
    ))
   

(defn deps-table
  "Write dependences to a CSV file.
   lein deps command filename [options]
   command is :dependencies or :managed-dependencies.
   By default, writes only top-level dependencies, but
   if you add :deep true to the command line, you'll get
   the complete dependency tree, flattened and sorted."
  [project & [command filename & args]]
  (let [project (project/merge-profiles
                 project
                 [{:pedantic? (quote ^:displace warn)}])
        options (apply hash-map args)
        [dependencies-key managed-dependencies-key] (deps-command command [:dependencies :managed-dependencies])
        deep? (get options ":deep")
        hierarchy (if deep?
                    (classpath/managed-dependency-hierarchy
                     dependencies-key
                     managed-dependencies-key
                     project))
        rows (map rowify (if deep? (flatten-deps hierarchy) (dependencies-key project)))]
    (with-open [writer (io/writer filename)]
      (csv/write-csv writer
                     (concat [["Group/Artifact" "Version" "Exclusions" "Classifier"]]
                             rows)))))
                       
                     


