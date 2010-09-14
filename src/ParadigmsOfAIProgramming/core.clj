(ns ParadigmsOfAIProgramming.core)

(defn mappend [f lst]
  (apply concat (map f lst)))


;Chapter 2: A simple Lisp Programm
;A strait forword solution
(defn list-rand-nth [lst] (list (rand-nth lst)))

(defn Verb [] (list-rand-nth '(hit took saw liked)))
(defn Noun [] (list-rand-nth '(man ball woman table)))
(defn Article [] (list-rand-nth '(a the)))
(defn noun-phrase [] (concat (Article) (Noun))) 
(defn verb-phrase [] (concat (Verb) (noun-phrase)))
(defn sentence [] (concat (noun-phrase) (verb-phrase)))

;A rule based solution

(def *simple-grammer*
     '((sentence -> (noun-phrase verb-phrase))
       (Article -> a the)
       (noun-phrase -> (Article Noun))
       (verb-phrase -> (Verb noun-phrase))
       (Noun -> man ball woman table)
       (Verb -> hit took saw liked)
      "A grammer for a trivial subset of English"))

(def *grammer* (atom *simple-grammer*))

(defn get-rule [rule gram]
  (first (filter #(= (first %) rule) gram)))

(defn rule-lhs [rule]
  (first rule))
(defn rule-rhs [rule]
  (rest (rest rule)))
(defn rewrites [category]
  (rule-rhs (get-rule category @*grammer*)))

(defn generate
  "Generat a random sentence or phrase"
  [phrase]
  (let [writs (rewrites phrase)]
   (cond (list? phrase) (mappend generate phrase)
         (seq writs) (generate writs)
         :default (list phrase))))
