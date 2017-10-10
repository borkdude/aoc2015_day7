;; Solution to day 7 of Advent of Code using clojure.spec
;; Problem description: http://adventofcode.com/day/7

(ns day7
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as t]
            [clojure.spec.gen.alpha :as gen]))

(def varname-gen
  (gen/fmap (fn [chars]
              (symbol
               (str/lower-case
                (apply str chars))))
            (gen/vector (gen/char-alpha)
                        1
                        2)))

(s/def ::varname
  (s/with-gen symbol?
    (fn [] varname-gen)))

(comment
  (gen/sample varname-gen) ;;=> (g dv de pw hi a c j bz y)
  )

(s/def ::val (s/alt :name ::varname
                    :value nat-int?))
(s/def ::binary-operator #{'LSHIFT 'RSHIFT 'AND 'OR})
(s/def ::binary-expression (s/cat
                            :left-operand ::val
                            :operator ::binary-operator
                            :right-operand ::val))
(s/def ::not (s/cat :not #{'NOT} :operand ::val))
(s/def ::lhs (s/alt :simple-value ::val
                    :binary-expression
                    ::binary-expression
                    :not ::not))
(s/def ::rhs ::varname)
(s/def ::expr (s/cat :lhs ::lhs :arrow #{'->} :rhs ::rhs))

(comment
  (gen/sample (s/gen ::expr)) ;;=> ((NOT g -> sw) (NOT 0 -> j) (gl LSHIFT 0 -> q) (NOT 1 -> ly) (NOT 2 -> j) (ug -> o) (p RSHIFT 0 -> p) (NOT oj -> dz) (ih -> m) (NOT 5 -> fc))
  )

(defn get-lines []
  (str/split-lines
   (slurp "input-day7.txt")))

(defn parsed-lines [lines]
  (mapv (fn [l]
          (let [edn (edn/read-string
                     (format "[%s]" l))]
            (s/conform ::expr edn)))
        lines))

(comment
  (first (parsed-lines (get-lines))) ;;=> {:lhs [:binary-expression {:left-operand [:name bn], :operator RSHIFT, :right-operand [:value 2]}], :arrow ->, :rhs bo}
  )

(def context (atom {}))

(defn value-by-symbol [sym]
  @(get @context sym))

(defn evaluate* [[kind tree-or-val]]
  (case kind
    :value tree-or-val
    :name (value-by-symbol tree-or-val)
    :simple-value (evaluate* tree-or-val)
    :not (bit-not
          (evaluate*
           (:operand tree-or-val)))
    :binary-expression
    (let [l (evaluate* (:left-operand tree-or-val))
          r (evaluate* (:right-operand tree-or-val))
          operator (case (:operator tree-or-val)
                     AND bit-and
                     OR bit-or
                     LSHIFT bit-shift-left
                     RSHIFT bit-shift-right)]
      (operator l r))))

(defn evaluate-expr! [expr]
  (let [rhs (:rhs expr)
        lhs (:lhs expr)]
    (swap! context assoc rhs
           (delay
            (evaluate* lhs)))))

(defn evaluate-lines! [lines]
  (doseq [expr (parsed-lines lines)]
    (evaluate-expr! expr)))

(comment
  ;; part 1
  (evaluate-lines! (get-lines))
  (def answer-part-1 (value-by-symbol 'a))
  ;; part 2
  (evaluate-lines! (get-lines))
  (swap! context assoc 'b (delay answer-part-1))
  (def answer-part-2 (value-by-symbol 'a)))
