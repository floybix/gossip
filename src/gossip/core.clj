(ns gossip.core
  (:gen-class)
  (:require [clojure.set :as set]
            [clojure.core.match :refer [match]]))

(defn empty-state
  "The whole game state is a map with keys for each character, storing
  their personal beliefs. Each character's beliefs are structured like

  {source {target feeling}}

  where source and target are character ids, and feeling is a
  keyword, :like / :fear / :anger / :none."
  [characters]
  (let [target-beliefs {}
        personal-beliefs (zipmap characters (repeat target-beliefs))]
    (->
     (zipmap characters (repeat personal-beliefs))
     (assoc ::time 0
            ::characters (set characters)))))

(defn belief-seq
  [beliefs]
  (for [[source m] beliefs
        [target feeling] m]
    [source target feeling]))

(defn choose-gossip
  "Character 'speaker' tells another character 'listener' one of their
  beliefs. It should be something that listener does not already
  know.

  (?) If the listener already believes something incompatible, then the
  _later_ belief wins and is communicated back to the speaker. (?)"
  [state speaker listener]
  (let [spe-beliefs (get state speaker)
        lis-beliefs (get state listener)
        news (set/difference (set (belief-seq spe-beliefs))
                             (set (belief-seq lis-beliefs)))]
    (if (seq news)
      (rand-nth (seq news))
      nil)))

(defn learn
  [beliefs belief]
  (let [[source target feeling] belief]
    (assoc-in beliefs [source target] feeling)))

(defn felt-for-who
  [feeling ones-beliefs]
  (keep (fn [[target x]]
          (when (= x feeling) target))
        ones-beliefs))

;;; ## Rules

(defn dont-fear-each-other
  [beliefs me you]
  (for [other (felt-for-who (get beliefs me) :fear)
        :let [their-feeling (get-in beliefs [other me])]
        :when (= :fear their-feeling)]
    [me other :none]))

(defn like-back
  [beliefs me you]
  (for [other (keys (dissoc beliefs me))
        :let [their-feeling (get-in beliefs [other me])]
        :when (= :like their-feeling)
        :let [my-feeling (get-in beliefs [me other])]
        :when (not= :like my-feeling)]
    [me other :like]))

(defn anger-response
  [beliefs me you]
  (for [other (keys (dissoc beliefs me))
        :when (= :anger (get-in beliefs [other me]))
        :let [my-feeling (get-in beliefs [me other])]
        :when (not (contains? #{:anger :fear} my-feeling))
        :let [new-feeling (rand-nth [:anger :fear])]]
    [me other new-feeling]))

(defn jealousy-or-grouping
  [beliefs me]
  (for [my-like (felt-for-who (get beliefs me) :like)
        their-like (felt-for-who (get beliefs my-like) :like)
        :when (not= their-like me)
        :let [my-feeling (get-in beliefs [me their-like])]
        :when (not (contains? #{:anger :like} my-feeling))
        :let [new-feeling (rand-nth [:anger :like])]]
    [me their-like new-feeling]))

(defn loyalty
  [beliefs me]
  (for [my-like (felt-for-who (get beliefs me) :like)
        their-fear (felt-for-who (get beliefs my-like) :fear)
        :when (not= their-fear me)
        :let [my-feeling (get-in beliefs [me their-fear])]
        :when (not (contains? #{:anger :fear} my-feeling))
        :let [new-feeling (rand-nth [:anger :fear])]]
    [me their-fear new-feeling]))

(def responses
  [dont-fear-each-other
   like-back
   anger-response
   jealousy-or-grouping
   loyalty])

;;;; ## end of rules

(defn think
  [beliefs me]
  (loop [responses responses
         beliefs beliefs
         news []]
    (if-let [r (first responses)]
      (let [r-news (r beliefs me)]
        (recur (rest responses)
               (reduce learn beliefs r-news)
               (into news r-news)))
      ;; done
      [beliefs news])))

(defn meet
  [state speaker listener]
  (let [gossip (choose-gossip state speaker listener)]
    (if gossip
      (let [beliefs (get state listener)
            new-b (learn beliefs gossip)
            [new-b-2 news-2] (think new-b listener)
            [new-b-3 news-3] (think new-b-2 listener)
            new-state (assoc state listener new-b-3)]
        [new-state {:speaker speaker
                    :listener listener
                    :gossip gossip
                    :thoughts [news-2 news-3]}])
      ;; no gossip
      [state {}])))

(defn random-meeting
  [state]
  (let [chars (::characters state)
        speaker (rand-nth (seq chars))
        listener (rand-nth (seq (disj chars speaker)))]
    (meet state speaker listener)))

(defn step
  [state]
  (let [[new-state stuff] (random-meeting state)]
    (println stuff)
    (update new-state
            ::time inc)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
