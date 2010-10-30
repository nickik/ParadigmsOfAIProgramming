(ns ParadigmsOfAIProgramming.core
  (use [clojure.set])
  (use [clojure.pprint]))

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
     '((Sentence -> (noun-phrase verb-phrase))
       (Article -> a the)
       (noun-phrase -> (Article Noun))
       (verb-phrase -> (Verb noun-phrase))
       (Noun -> man ball woman table)
       (Verb -> hit took saw liked)
      "A grammer for a trivial subset of English"))

(defn get-rule [rule gram]
  (first (filter #(= (first %) rule) gram)))

(defn rule-lhs [rule]
  (first rule))
(defn rule-rhs [rule]
  (rest (rest rule)))

(defn rewrites [grammer category]
  (rule-rhs (get-rule category grammer)))

(defn generate
  "Generat a random sentence or phrase"
  [grammer phrase]
  #_(println "phrase " phrase)
  (cond  (list? phrase) (apply concat (map (partial generate grammer) phrase))
         (seq (rewrites grammer phrase))
         (generate grammer (rand-nth (rewrites grammer phrase)))
         :default (list phrase)))

(defn simple-generat [phrase]
  (generate *simple-grammer* phrase))

;2.5 Changing the Grammer without Changing the Programm

(def *bigger-grammer*
     '((Sentence -> (noun-phrase verb-phrase))
       (Article -> a the)
       (noun-phrase -> (Article Adj* Noun PP*))
       (verb-phrase -> (Verb noun-phrase PP*))
       (Noun -> man ball woman table)
       (Verb -> hit took saw liked)
       (PP* -> () (PP PP*))
       (Adj* -> () (Adj Adj*))
       (PP -> () (Prep noun-phrase))
       (Prep -> to in by with on)
       (Adj -> big little blue green adiabatic sexy red)
       (Name -> Michael Therry Nick Kevin Marcel)
       (Pronoun -> he she it these those that)
       "A grammer for a trivial subset of English"))

(defn bigger-generat [phrase]
  (generate *bigger-grammer* phrase))

;2.6 Using the Same Data for Several Programs

(defn generate-tree
"Generate a random senence or phrase"
  [phrase]
  (cond (list? phrase) (map generate-tree phrase)
        (seq (rewrites *simple-grammer* phrase))
          (cons phrase (generate-tree (rand-nth (rewrites *simple-grammer* phrase))))
          :default (list phrase)))


(defn my-combine-all
  "Return a list of lists formed by appending a y to an x"
  [xlist ylist]
  (for [y ylist
        x xlist]
    [x y]))
(defn combine-all [xlist ylist]
  (apply concat (map (fn [y] (map (fn [x] (concat x y)) xlist)) ylist)))

(defn generate-all
  [phrase]
  (cond (nil? phrase) (list nil)
        (list? phrase) (combine-all (generate-all (first phrase))
                                    (generate-all (next phrase)))
        (seq (rewrites *simple-grammer* phrase)) (apply concat
                                                    (map generate-all
                                                         (rewrites *simple-grammer*
                                                                   phrase)))
        :default (list (list phrase))))

;My easy programm OO programming lang grammer

(def *class-grammer*
     '((Programm -> (namespace_ namespaces*))
       (namespaces* -> () (namespace_ namespaces*))
       (namespace_ -> (nsname classes* functions*))
       (nsname -> core contrib extendlib xml json)
       (classes* -> () (class_ classes*))
       (class_ -> (classname slots* methods*))
       (classname -> (object type))
       (functions* -> () (functions functions*))
       (functions -> print prn add subtract multi div modulo)
       (slots* -> () (slots slots*))
       (slots -> zwischenresultat irgendein-unnötiger-state)
       (methods* -> () (methods_ methods*))
       (methods_ -> getter setter IOmetod)
       "Grammer of a liddle OO language"))

(defn class-generat [phrase]
  (generate *class-grammer* phrase))

;Exersise 2.4
(defn cross-product [f xcoll ycoll]
  (mapcat (fn [y] (map (fn [x] (f x y)) xcoll)) ycoll))

(defn combine-all [xlist ylist]
  (cross-product concat xlist ylist))

;Chapter 3

(defn remove= [item coll]
  (remove (partial = item) coll))

(defn find-all2 [item coll pred]
  (remove (complement (partial pred item)) coll))


; http://groups.google.com/group/clojure/browse_thread/thread/d4ccbfe2a62c827f
(defn find-all [item coll & {:keys [test test-not] :or {test =}}] 
  (if test-not 
    (remove #(test-not item %) coll) 
    (filter #(test item %) coll))) 

;writen by me
(defn my-member [item coll]
  (loop [coll coll]
    (when coll
      (if (= item (first coll))
       coll
       (recur (next coll))))))

;from the nice #clojure channel
(defn member [item coll]
  (drop-while (complement #{item}) coll))

; 4 .GPS

; 4.2 Specification

 ;- since clojure has set I hope that helps me. 
 ;   - a condition is a symbol  
 ;      - CL -> (rich famous) (unknown poor)
 ;      - Clojure -> #{rich famous} #{unknown poor}

 ; List of poerators witch is consisten for a problem
 ; I am going to take atom here because we will have tocange it

 ; (GPS #{rich famous} #{unknow poor} list-of-ops)
;implementation

(def *ops* (atom [])) 
(def *state* (atom #{}))

(defrecord op [action preconds add-list del-list])

(defn appropriate? [goal op]
  "An op is appropriate to a goal if it is in its add list"
  (some #{goal} (:add-list op)))

(defn achived? [goal]
  (or (some #{goal} *state*)
      (some apply-op (find-all goal *ops* :test appropriate?))))

(defn apply-op [op]
  (when (every? achived? (:preconds op))
    #_(println (str "(Exectuting " (:action op) ")"))
    (swap! *state* (difference @*state* (:del-list op)))
    (swap! *state* (union @*state* (:add-list op)))
    true))

(defn GPS [goals state op]
  (reset! *state* state)
  (reset! *ops* op)
  (let [re (if (every? achived? goals) "solved")]
    (reset! *state* [])
    (reset! *ops* [])
    re))

;wwww.norvig.com/paip/gps1.lisp
;;action preconds add-list del-list

(def *school-ops*
     [(op. 'drive-son-to-school
           #{'son-at-home 'car-works}
           #{'son-at-school}
           #{'son-at-home})
      (op. 'shop-installs-battery
           #{'car-needs-battery 'shop-knows-problem 'shop-has-money}
           #{'car-works}
           nil)
      (op. 'tell-shop-problem
           #{'in-communication-with-shop}
           #{'shop-knows-problem}
           nil)
      (op. 'telephone-shop
           #{'know-phone-number}
           #{'in-communication-with-shop}
           nil)
      (op. 'look-up-number
           #{'have-phone-book}
           #{'know-phone-number}
           nil)
      (op. 'give-shop-money
           #{'have-money}
           #{'shop-has-money}
           #{'have-money})])

;; TEST Function for the GPS
(GPS #{'son-at-home 'car-works} '(son-at-school) *school-ops*)
(GPS #{'son-at-home 'car-needs-battery 'have-money 'have-phone-book}
        '(son-at-school)
        *school-ops*)

