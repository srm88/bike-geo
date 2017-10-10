#!/bin/bash
# Kludgy script, I'm too lazy to sort out building an executable class from clj right now.
cat <<EOF | java -classpath "$(pwd):/Users/smerritt/Downloads/clojure-1.8.0.jar" clojure.main
(require '[bike-geo :refer :all])
(big-report)
EOF
