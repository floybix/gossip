(ns gossip.core
  (:require [datascript.core :as d]
            [clojure.set :as set]
            [clojure.string :as str]
            #?(:cljs [goog.string :as gstring :refer [format]])
            #?(:cljs [goog.string.format])))

(def db-schema
  {:person/id {:db/doc "A character's keyword id, used in beliefs."
               :db/cardinality :db.cardinality/one
               :db/unique :db.unique/identity}
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
  [db belief]
  (d/q '[:find (pull ?e [*]) .
         :in $ [?mind ?x ?y]
         :where
         [?e :belief/mind ?mind]
         [?e :relation/subject ?x]
         [?e :relation/object ?y]]
       db
       (map belief [:belief/mind
                    :relation/subject
                    :relation/object])))

(defn choose-belief-for-gossip
  [db speaker listener]
  (let [bs (d/q feelings-q db speaker)]
    (loop [bs (shuffle bs)]
      (if-let [belief (first bs)]
        ;; check whether listener already knows this
        (let [their (assoc belief :belief/mind listener)
              their-existing-belief (existing-relation db their)]
          (if (= (:relation/feeling their-existing-belief)
                 (:relation/feeling belief))
            ;; already known, continue
            (recur (rest bs))
            ;; got one
            belief))
        ;; checked all, no new gossip
        nil))))

(defn updatable-relation
  [db new-belief]
  (let [existing (existing-relation db new-belief)
        eid (or (:db/id existing) -1)]
    (assoc new-belief :db/id eid)))

(defn believe
  ([db mind subject object feeling]
   (let [belief {:belief/mind mind
                 :relation/subject subject
                 :relation/object object
                 :relation/feeling feeling}]
     (believe db belief)))
  ([db belief]
   (d/db-with db [(updatable-relation db belief)])))

(def dbrules
  '[[(feels-for ?e ?mind ?subject ?object ?feeling)
     [?e :belief/mind ?mind]
     [?e :relation/subject ?subject]
     [?e :relation/object ?object]
     [?e :relation/feeling ?feeling]
     ]
    ])

(defn replacem
  [s replacements]
  (reduce (fn [s [from to]]
            (str/replace s from to))
          s replacements))

(defn him-her
  [db person]
  (case (-> (d/pull db [:person/gender] [:person/id person])
            :person/gender)
    :male "him"
    :female "her"))

;;; ## Responses

(defn mutual-fear-response
  [db mind]
  (let [ans (d/q '[:find ?x ?e1 ?e2
                   :in $ ?mind %
                   :where
                   (feels-for ?e1 ?mind ?mind ?x :fear)
                   (feels-for ?e2 ?mind ?x ?mind :fear)
                   ]
                 db mind dbrules)]
    (for [[x e1 e2] ans]
      {:db/id e1
       :relation/feeling :none
       :belief/cause e2
       :belief/phrase
       (replacem "Huh? You mean FOO is afraid of me?? Well I shouldn't be afraid of them any more!"
                 {"FOO" (name x)})})))

(defn x-like-me-response
  [db mind]
  (let [ans (d/q '[:find ?x ?e1
                   :in $ ?mind %
                   :where
                   (feels-for ?e1 ?mind ?x ?mind :like)
                   ]
                 db mind dbrules)]
    (for [[x e1] ans
          :let [relation {:belief/mind mind
                          :relation/subject mind
                          :relation/object x}
                existing (existing-relation db relation)]
          :when (not= :like (:relation/feeling existing))]
      (assoc relation
             :db/id (or (:db/id existing) -1)
             :relation/feeling :like
             :belief/cause e1
             :belief/phrase
             (replacem "FOO likes me! We can be friends."
                       {"FOO" (name x)})))))

(defn x-angry-me-response
  [db mind]
  (let [ans (d/q '[:find ?x ?e1
                   :in $ ?mind %
                   :where
                   (feels-for ?e1 ?mind ?x ?mind :anger)
                   ]
                 db mind dbrules)]
    (for [[x e1] ans
          :let [relation {:belief/mind mind
                          :relation/subject mind
                          :relation/object x}
                existing (existing-relation db relation)]
          :when (not (contains? #{:anger :fear} (:relation/feeling existing)))
          :let [new-feeling (rand-nth [:anger :fear])]]
      (assoc relation
             :db/id (or (:db/id existing) -1)
             :relation/feeling new-feeling
             :belief/cause e1
             :belief/phrase
             (cond
               (= :anger new-feeling)
               (replacem "Well if FOO is angry with me then I'm angry too."
                         {"FOO" (name x)})
               (= :fear new-feeling)
               (replacem "Uh-oh, I'd better keep away from FOO."
                         {"FOO" (name x)}))))))

(defn me-like-x-&-x-like-y-response
  [db mind]
  (let [ans (d/q '[:find ?x ?other ?e2
                   :in $ ?mind %
                   :where
                   (feels-for ?e1 ?mind ?mind ?x :like)
                   (feels-for ?e2 ?mind ?x ?other :like)
                   [(not= ?other ?mind)]
                   ]
                 db mind dbrules)]
    (for [[x other e2] ans
          :let [relation {:belief/mind mind
                          :relation/subject mind
                          :relation/object other}
                existing (existing-relation db relation)]
          :when (not (contains? #{:anger :like} (:relation/feeling existing)))
          :let [new-feeling (rand-nth [:anger :like])]]
      (assoc relation
             :db/id (or (:db/id existing) -1)
             :relation/feeling new-feeling
             :belief/cause e2
             :belief/phrase
             (cond
               (= :anger new-feeling)
               (replacem "Oh, why does SUBJ like OBJ? I want SUBJ to
                             only like me. Now I'm angry with OBJ."
                         {"SUBJ" (name x), "OBJ" (name other)})
               (= :like new-feeling)
               (replacem "Great, SUBJ likes OBJ so they must be fun,
                             I'll be friends with them too."
                         {"SUBJ" (name x), "OBJ" (name other)}))))))

(defn me-like-x-&-x-fear-y-response
  [db mind]
  )

(defn me-like-x-&-y-angry-x-response
  [db mind]
  )

(defn me-fear-x-&-y-angry-x-response
  [db mind]
  )

(defn me-angry-x-&-y-angry-x-response
  [db mind]
  )

(def all-response-fns
  [mutual-fear-response
   x-like-me-response
   x-angry-me-response
   me-like-x-&-x-like-y-response])

(comment
  (use 'clojure.pprint)
  (def conn (atom (-> (d/empty-db db-schema)
                      (d/db-with (people-datoms {:fred :male
                                                 :ethel :female
                                                 :sam :male})))))
  (swap! conn believe :fred :fred :ethel :like)
  (swap! conn believe :fred :fred :sam :like)
  (pprint (d/q feelings-q @conn :fred))
  (d/transact! conn (mutual-fear-response conn :fred))
  (mutual-fear-response @conn :fred)

  (pprint (d/q feelings-q @conn :fred))
)

(defn derive-beliefs
  [db mind]
  (mapcat (fn [response-fn]
            (response-fn db mind))
          all-response-fns))

(def meeting-phrases
  ["At a party that night, SPE walks over to LIS and tells HIM/HER:"
   "The next day, SPE sees LIS at the park and tells HIM/HER:"
   "In maths class, SPE passes a note to LIS which says:"])

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
  [db belief speaker listener]
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
  "Returns keys :gossip :message :meet-phrase"
  [db speaker listener]
  (if-let [belief (choose-belief-for-gossip db speaker listener)]
    (let [goss (-> belief
                   (select-keys [:relation/subject
                                 :relation/object
                                 :relation/feeling])
                   (assoc :belief/mind listener
                          :belief/source speaker))
          ;; TODO: we choose only unknown or contradictory info... what about confirmatory?
          ;; TODO: what if belief subject is the listener? a confrontation.
          message-prefix (rand-nth message-prefixes)
          message-str (phrase-belief db belief speaker listener)
          explain-str (if-let [cause-e (:belief/cause belief)]
                        ;; look up reason
                        (let [cause (d/pull db '[*] (:db/id cause-e))]
                          (str "It all started because "
                               (phrase-belief db cause speaker listener)))
                        ;; not my own thought; I heard it from someone
                        (if-let [source (:belief/source belief)]
                          (cond
                            (= source (:relation/subject belief))
                            (replacem "FOO told me HIM/HERself."
                                      {"FOO" (name source)
                                       "HIM/HER" (him-her db source)})
                            :else
                            (format "I heard it from %s."
                                    (name source)))
                          ;; there is neither a cause nor a source
                          ""))
          meet-str (-> (rand-nth meeting-phrases)
                       (replacem {"SPE" (name speaker)
                                  "LIS" (name listener)
                                  "HIM/HER" (him-her db listener)}))
          ]
      {:meet-phrase meet-str
       :message (str/join \newline [message-prefix message-str explain-str])
       :gossip [goss]})
    ;; no news
    ;; TODO: become friends? spontaneous like
    nil
    ))

(defn all-people
  [db]
  (d/q '[:find [?person ...]
         :where
         [_ :person/id ?person]]
       db))

(defn random-meet
  [db]
  (let [people (all-people db)
        speaker (rand-nth people)
        listener (rand-nth (remove #(= speaker %) people))
        foo (meet db speaker listener)]
    (assoc foo
           :speaker speaker
           :listener listener)
    ))

(defn think
  "Returns [db thoughts], where thoughts are any belief maps just
  added to the database."
  [db mind]
  (let [thoughts (derive-beliefs db mind)]
    [(if (seq thoughts)
       (d/db-with db thoughts)
       db)
     thoughts]))

(defn step
  "Returns keys
  :db :speaker :listener :gossip :meet-phrase :message :thoughts :reply
  or nil if no gossip."
  [db]
  (let [meeting (random-meet db)
        {:keys [speaker listener gossip meet-phrase message]} meeting]
    (when (seq gossip)
      (let [db (d/db-with db gossip)
            [db thoughts1] (think db listener)]
        (if thoughts1
          (let [;; there might be more thoughts that can derived now:
                [db thoughts2] (think db listener)
                ;; there might be even more thoughts that can derived:
                [db thoughts3] (if thoughts2
                                 (think db listener)
                                 [db nil])
                ;; ok let's not go crazy... stop now.
                reply (if (contains? #{:anger :fear}
                                     (:relation/feeling (first thoughts1)))
                        (rand-nth negative-response-phrases)
                        (rand-nth positive-response-phrases))]
            (assoc meeting
                   :db db
                   :thoughts (concat thoughts1 thoughts2 thoughts3)
                   :reply reply))
          ;; no derived thoughts
          (let [reply (rand-nth positive-response-phrases)]
            (assoc meeting
                   :db db
                   :reply reply)))))))
