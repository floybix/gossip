(ns gossip.core
  (:gen-class)
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [datascript.core :as d]))

(def db-schema
  {:person/id {:db/doc "A character's keyword id, used in beliefs."
               :db/cardinality :db.cardinality/one
               :db/unique :db.unique/value}
   :person/gender {:db/doc "A character's sex, :female or :male"
                   :db/cardinality :db.cardinality/one}
   :belief/mind {:db/doc "Which character's mind this belief is held in."
                 :db/cardinality :db.cardinality/one}
   :belief/cause {:db/doc "Which other belief led to this belief."
                  :db/cardinality :db.cardinality/one
                  :db/valueType :db.type/ref}
   :belief/source {:db/doc "Who did I learn this from."
                   :db/cardinality :db.cardinality/one}
   :belief/phrase {:db/doc "An english phrase expressing this belief/feeling."
                    :db/cardinality :db.cardinality/one}
   :relation/subject {:db/doc "Which character is the subject of this relationship, eg the angry one."
                      :db/cardinality :db.cardinality/one}
   :relation/object {:db/doc "Which character is the object of this relationship, eg the one angry with."
                     :db/cardinality :db.cardinality/one}
   :relation/feeling {:db/doc "What is the feeling of this relationship, eg anger."
                      :db/cardinality :db.cardinality/one}
   })

(defn people-datoms
  "The datoms listing each character that will be in the
  simulation. Give a map of keywords to gender keywords."
  [character-map]
  (for [[id g] character-map]
    {:person/id id
     :person/gender g}))

(def feelings-q
  "Query to pull out all relationship belief entities, parameterised by mind."
  '[:find [(pull ?e [*]) ...]
    :in $ ?mind
    :where
    [?e :belief/mind ?mind]
    [?e :relation/feeling]
    ])

(defn existing-relation
  [conn belief]
  (d/q '[:find (pull ?e [*]) .
         :in $ [?mind ?x ?y]
         :where
         [?e :belief/mind ?mind]
         [?e :relation/subject ?x]
         [?e :relation/object ?y]]
       @conn
       (map belief [:belief/mind
                    :relation/subject
                    :relation/object])))

(defn choose-belief-for-gossip
  [conn speaker listener]
  (let [bs (d/q feelings-q @conn speaker)]
    (loop [bs (shuffle bs)]
      (if-let [belief (first bs)]
        ;; check whether listener already knows this
        (let [their (assoc belief :belief/mind listener)
              their-existing-belief (existing-relation conn their)]
          (if (= (:relation/feeling their-existing-belief)
                 (:relation/feeling belief))
            ;; already known, continue
            (recur (rest bs))
            ;; got one
            belief))
        ;; checked all, no new gossip
        nil))))

(defn updatable-relation
  [conn new-belief]
  (let [existing (existing-relation conn new-belief)
        eid (or (:db/id existing) -1)]
    (assoc new-belief :db/id eid)))

(defn believe!
  ([conn mind subject object feeling]
   (let [belief {:belief/mind mind
                 :relation/subject subject
                 :relation/object object
                 :relation/feeling feeling}]
     (believe! conn belief)))
  ([conn belief]
   (d/transact! conn [(updatable-relation conn belief)])))

(def dbrules
  '[[(feels-for ?e ?mind ?subject ?object ?feeling)
     [?e :belief/mind ?mind]
     [?e :relation/subject ?subject]
     [?e :relation/object ?object]
     [?e :relation/feeling ?feeling]
     ]
    ])

;;; ## Responses

(defn mutual-fear-response
  [conn mind]
  (let [ans (d/q '[:find ?x ?e1 ?e2
                   :in $ ?mind %
                   :where
                   (feels-for ?e1 ?mind ?mind ?x :fear)
                   (feels-for ?e2 ?mind ?x ?mind :fear)
                   ]
                 @conn mind dbrules)]
    (for [[x e1 e2] ans]
      {:db/id e1
       :relation/feeling :none
       :belief/cause e2
       :belief/phrase
       (format "Huh? You mean %s is afraid of me?? Well I shouldn't be afraid of them any more!"
               (name x))})))

(defn x-like-me-response
  [conn mind]
  (let [ans (d/q '[:find ?x ?e1
                   :in $ ?mind %
                   :where
                   (feels-for ?e1 ?mind ?x ?mind :like)
                   ]
                 @conn mind dbrules)]
    (for [[x e1] ans
          :let [relation {:belief/mind mind
                          :relation/subject mind
                          :relation/object x}
                existing (existing-relation conn relation)]
          :when (not= :like (:relation/feeling existing))]
      (assoc relation
             :db/id (or (:db/id existing) -1)
             :relation/feeling :like
             :belief/cause e1
             :belief/phrase
             (format "%s likes me! We can be friends."
                     (name x))))))

(defn x-angry-me-response
  [conn mind]
  (let [ans (d/q '[:find ?x ?e1
                   :in $ ?mind %
                   :where
                   (feels-for ?e1 ?mind ?x ?mind :anger)
                   ]
                 @conn mind dbrules)]
    (for [[x e1] ans
          :let [relation {:belief/mind mind
                          :relation/subject mind
                          :relation/object x}
                existing (existing-relation conn relation)]
          :when (not (contains? #{:anger :fear} (:relation/feeling existing)))
          :let [new-feeling (rand-nth [:anger :fear])]]
      (assoc relation
             :db/id (or (:db/id existing) -1)
             :relation/feeling new-feeling
             :belief/cause e1
             :belief/phrase
             (cond
               (= :anger new-feeling)
               (format "Well if %s is angry with me then I'm angry too."
                       (name x))
               (= :fear new-feeling)
               (format "Uh-oh, I'd better keep away from %s."
                       (name x)))))))

(defn me-like-x-&-x-like-y-response
  [conn mind]
  (let [ans (d/q '[:find ?x ?other ?e2
                   :in $ ?mind %
                   :where
                   (feels-for ?e1 ?mind ?mind ?x :like)
                   (feels-for ?e2 ?mind ?x ?other :like)
                   [(not= ?other ?mind)]
                   ]
                 @conn mind dbrules)]
    (for [[x other e2] ans
          :let [relation {:belief/mind mind
                          :relation/subject mind
                          :relation/object other}
                existing (existing-relation conn relation)]
          :when (not (contains? #{:anger :like} (:relation/feeling existing)))
          :let [new-feeling (rand-nth [:anger :like])]]
      (assoc relation
             :db/id (or (:db/id existing) -1)
             :relation/feeling new-feeling
             :belief/cause e2
             :belief/phrase
             (cond
               (= :anger new-feeling)
               (format "Oh, why does %1$s like %2$s? I want %1$s to only like me. Now I'm angry with %2$s."
                       (name x) (name other))
               (= :like new-feeling)
               (format "Great, %1$s likes %2$s so they must be fun, I'll be friends with them too."
                       (name x) (name other)))))))

(defn me-like-x-&-x-fear-y-response
  [conn mind]
  )

(defn me-like-x-&-y-angry-x-response
  [conn mind]
  )

(defn me-fear-x-&-y-angry-x-response
  [conn mind]
  )

(defn me-angry-x-&-y-angry-x-response
  [conn mind]
  )

(def all-response-fns
  [mutual-fear-response
   x-like-me-response
   x-angry-me-response
   me-like-x-&-x-like-y-response])

(comment
  (use 'clojure.pprint)
  (def conn (d/create-conn db-schema))
  (d/transact! conn (people-datoms {:fred :male
                                    :ethel :female
                                    :sam :male}))
  (believe! conn :fred :fred :ethel :like)
  (believe! conn :fred :fred :sam :like)
  (pprint (d/q feelings-q @conn :fred))
  (d/transact! conn (mutual-fear-response conn :fred))
  (mutual-fear-response conn :fred)

  (pprint (d/q feelings-q @conn :fred))
)

(defn derive-beliefs
  [conn mind]
  (mapcat (fn [response-fn]
            (response-fn conn mind))
          all-response-fns))

(def meeting-phrases
  ["At a party that night, %s walks over to %s and tells HIM/HER:"
   "The next day, %s sees %s at the park and tells HIM/HER:"
   "In maths class, %s passes a note to %s which says:"])

(def message-prefixes
  ["Did you know?"
   "I wanted to tell you,"
   "OMG I can't believe it!"])

(def positive-response-phrases
  ["That's cool."
   "Oh wow."])

(def negative-response-phrases
  ["That sucks."
   "Oh man, that's just like them."])

(defn phrase-belief
  [conn belief speaker listener]
  (let [subj (:relation/subject belief)
        obj (:relation/object belief)
        feel (:relation/feeling belief)
        my-feeling? (= subj speaker)
        your-feeling? (= subj listener)
        about-me? (= obj speaker)
        about-you? (= obj listener)
        obj-name (cond
                   (= listener obj) "you"
                   (= speaker obj) "me"
                   :else (name obj))]
    (cond
      ;; my feeling
      (= subj speaker) ;; quite different grammar...
      (case feel
        :like (format "I like %s." obj-name)
        :anger (format "I'm angry with %s." obj-name)
        :fear (format "I'm afraid of %s." obj-name)
        (format "I'm %s with %s." feel obj-name))
      ;; your feeling
      (= subj listener)
      (case feel
        :like (format "you like %s." obj-name)
        :anger (format "you are angry with %s." obj-name)
        :fear (format "you are afraid of %s." obj-name)
        (format "you are %s with %s." feel obj-name))
      ;; someone else's feeling
      :else
      (case feel
        :like (format "%s likes %s." (name subj) obj-name)
        :anger (format "%s is angry with %s." (name subj) obj-name)
        :fear (format "%s is afraid of %s." (name subj) obj-name)
        (format "%s is %s with %s." (name subj) feel obj-name))
      )
    ))

(defn meet
  "Returns keys :gossip :narrative"
  [conn speaker listener]
  (if-let [belief (choose-belief-for-gossip conn speaker listener)]
    (let [goss (-> belief
                   (select-keys [:relation/subject
                                 :relation/object
                                 :relation/feeling])
                   (assoc :belief/mind listener
                          :belief/source speaker))
          ;; TODO: we choose only unknown or contradictory info... what about confirmatory?
          ;; TODO: what if belief subject is the listener? a confrontation.
          message-prefix (rand-nth message-prefixes)
          message-str (phrase-belief conn belief speaker listener)
          explain-str (if-let [cause-e (:belief/cause belief)]
                        ;; look up reason
                        (let [cause (d/pull @conn '[*] (:db/id cause-e))]
                          (str "It all started because "
                               (phrase-belief conn cause speaker listener)))
                        ;; not my own thought; I heard it from someone
                        (if-let [source (:belief/source belief)]
                          (cond
                            (= source (:relation/subject belief))
                            (format "%s told me HIM/HERself."
                                    (name source))
                            :else
                            (format "I heard it from %s."
                                    (name source)))
                          ;; there is neither a cause nor a source
                          ""))
          meet-str (-> (rand-nth meeting-phrases)
                       (format (name speaker) (name listener))
                       ;(str/replace)
                       )
          ]
      {:narrative [meet-str message-prefix message-str explain-str]
       :gossip [goss]})
    ;; no news
    ;; TODO: become friends? spontaneous like
    nil
    ))

(defn random-meet
  [conn]
  (let [people (d/q '[:find [?person ...]
                      :where
                      [_ :person/id ?person]]
                    @conn)
        speaker (rand-nth people)
        listener (rand-nth (remove #(= speaker %) people))
        foo (meet conn speaker listener)]
    (assoc foo
           :speaker speaker
           :listener listener)
    ))

(defn think!
  [conn mind]
  (let [thoughts (derive-beliefs conn mind)]
    (when (seq thoughts)
      (d/transact! conn thoughts)
      (println (format "%s thinks..." (name mind)))
      (doseq [b thoughts]
        (println (:belief/phrase b)))
      thoughts)))

(defn step!
  [conn]
  (let [meeting (random-meet conn)
        {:keys [speaker listener gossip narrative]} meeting]
    (when (seq gossip)
      (d/transact! conn gossip)
      (doseq [s narrative]
        (println s))
      (if-let [thoughts (think! conn listener)]
        (do
          ;; there might be more thoughts that can derived from these new ones:
          (when (think! conn listener)
            ;; there might be more thoughts that can derived from these new ones:
            (when (think! conn listener)
              ;; ok let's not go crazy here... stop.
              nil))
          (print (format "%s replies, " (name listener)))
          (if (contains? #{:anger :fear} (:relation/feeling (first thoughts)))
            (println (rand-nth negative-response-phrases))
            (println (rand-nth positive-response-phrases))))
        ;; no derived thoughts
        (do
          (print (format "%s replies, " (name listener)))
          (println (rand-nth positive-response-phrases)))))))

(defn -main
  [& args]
  (println "Hello, World!"))
