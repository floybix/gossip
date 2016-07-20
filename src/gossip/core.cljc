(ns gossip.core
  (:require [datascript.core :as d]
            [clojure.set :as set]
            [clojure.string :as str]
            #?(:clj [clojure.core.match :refer [match]]
               :cljs [cljs.core.match :refer-macros [match]])
            #?(:cljs [goog.string :as gstring :refer [format]])
            #?(:cljs [goog.string.format])))

(def db-schema
  {:person/id {:db/doc "A character's keyword id, used in beliefs."
               :db/cardinality :db.cardinality/one
               :db/unique :db.unique/identity}
   :person/gender {:db/doc "A character's sex, :female or :male"
                   :db/cardinality :db.cardinality/one}
   :debt/from {:db/doc "Who owes the debt."
               :db/index true}
   :debt/to {:db/doc "Who the debt is owed to."}
   :belief/person {:db/doc "Who holds this belief."
                   :db/cardinality :db.cardinality/one
                   :db/index true}
   :belief/mind {:db/doc "Who believes this? I.e. person believes that
                          mind thinks subject likes object."
                 :db/cardinality :db.cardinality/one
                 :db/index true}
   :belief/cause {:db/doc "Which other belief led to this belief."
                  :db/cardinality :db.cardinality/one
                  :db/valueType :db.type/ref}
   :belief/source {:db/doc "Who did I learn this from."
                   :db/cardinality :db.cardinality/one}
   :belief/original {:db/doc "Who was the original source."
                     :db/cardinality :db.cardinality/one}
   :belief/phrase {:db/doc "An english phrase expressing this belief/feeling."
                   :db/cardinality :db.cardinality/one}
   :belief/subject {:db/doc "Who feels the feeling, eg the angry one."
                    :db/cardinality :db.cardinality/one}
   :belief/object {:db/doc "Who is the feeling felt for, eg the one angry with."
                   :db/cardinality :db.cardinality/one}
   :belief/feeling {:db/doc "What is the feeling, eg anger."
                    :db/cardinality :db.cardinality/one}
   })

(defn people-datoms
  "The datoms listing each character that will be in the
  simulation. Give a map of keywords to gender keywords."
  [character-map]
  (for [[id g] character-map]
    {:person/id id
     :person/gender g}))

(def dbrules
  '[[(feels-for ?e ?person ?mind ?subject ?object ?feeling)
     [?e :belief/person ?person]
     [?e :belief/mind ?mind]
     [?e :belief/subject ?subject]
     [?e :belief/object ?object]
     [?e :belief/feeling ?feeling]
     ]
    ])

(def my-beliefs-q
  "Query to pull out all beliefs I personally hold. Parameter is
  person id."
  '[:find [(pull ?e [*]) ...]
    :in $ ?person
    :where
    [?e :belief/person ?person]
    [?e :belief/mind ?person]
    [?e :belief/feeling]
    ])

(def my-knowledge-of-their-beliefs-q
  "Query to pull out all beliefs that I think mind holds. The two
  parameters are person and whose mind they are considering."
  '[:find [(pull ?e [*]) ...]
    :in $ ?person ?mind
    :where
    [?e :belief/person ?person]
    [?e :belief/mind ?mind]
    [?e :belief/feeling]
    ])

(def my-feelings-q
  "Query to pull out all my feelings about other people. Parameter is
  person id."
  '[:find [(pull ?e [*]) ...]
    :in $ ?person
    :where
    [?e :belief/person ?person]
    [?e :belief/mind ?person]
    [?e :belief/subject ?person]
    [?e :belief/feeling]
    ])

(def perceived-popularity-q
  "Query to find how popular each character is (how many people like
  them), in the mind of a given character, as far as the query person
  knows. The two parameters are person id and whose mind they are
  considering."
  '[:find ?obj (count ?subj)
    :in $ ?person ?mind
    :where
    [?e :belief/person ?person]
    [?e :belief/mind ?mind]
    [?e :belief/subject ?subj]
    [?e :belief/object ?obj]
    [?e :belief/feeling :like]]
  )

(defn existing-belief
  [db belief]
  (d/q '[:find (pull ?e [*]) .
         :in $ [?person ?mind ?x ?y] %
         :where
         (feels-for ?e ?person ?mind ?x ?y _)]
       db
       (map belief [:belief/person
                    :belief/mind
                    :belief/subject
                    :belief/object])
       dbrules))

(defn find-news
  "Finds all beliefs that the listener holds but the speaker does not
  already know (what they actually know, not just what the speaker
  thinks they know). Returns a lazy sequence in random order.
  Leaves out anything the speaker feels directly for the listener!"
  [db speaker listener]
  ;; there are two cases:
  ;; 1. the listener has no record of the relationship.
  ;; 2. the listener knows of the relationship with different feeling.
  (let [bs (d/q '[:find [(pull ?e [*]) ...]
                  :in $ ?spe ?lis %
                  :where
                  (feels-for ?e ?spe ?spe ?subj ?obj _)
                  [(not (and (= ?subj ?spe)
                             (= ?obj ?lis)))]
                  ]
                db speaker listener dbrules)
        ]
    (->>
     (shuffle bs)
     (map (fn [belief]
            ;; check whether listener already knows this
            (let [their (assoc belief
                               :belief/person listener
                               :belief/mind listener)
                  their-existing-belief (existing-belief db their)]
              (if (= (:belief/feeling their-existing-belief)
                     (:belief/feeling belief))
                ;; already known, skip
                nil
                ;; got one
                belief))))
     (remove nil?))))

(defn updatable-belief
  [db new-belief]
  (let [existing (existing-belief db new-belief)]
    (if-let [eid (:db/id existing)]
      (assoc new-belief :db/id eid)
      (dissoc new-belief :db/id))))

(defn believe
  ([db person mind subject object feeling]
   (let [belief {:belief/person person
                 :belief/mind mind
                 :belief/subject subject
                 :belief/object object
                 :belief/feeling feeling}]
     (believe db belief)))
  ([db belief]
   (d/db-with db [(updatable-belief db belief)])))

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
  [db person mind]
  (let [ans (d/q '[:find ?x ?e1 ?e2
                   :in $ ?person ?mind %
                   :where
                   (feels-for ?e1 ?person ?mind ?mind ?x :fear)
                   (feels-for ?e2 ?person ?mind ?x ?mind :fear)
                   ]
                 db person mind dbrules)]
    (for [[x e1 e2] ans]
      {:db/id e1
       :belief/feeling :none
       :belief/cause e2
       :belief/phrase
       (replacem "Huh? You mean FOO is afraid of me?? Well I shouldn't be afraid of them any more!"
                 {"FOO" (name x)})})))

(defn x-like-me-response
  [db person mind]
  (let [ans (d/q '[:find ?x ?e1
                   :in $ ?person ?mind %
                   :where
                   (feels-for ?e1 ?person ?mind ?x ?mind :like)
                   ]
                 db person mind dbrules)]
    (for [[x e1] ans
          :let [belief {:belief/person person
                        :belief/mind mind
                        :belief/subject mind
                        :belief/object x}
                existing (existing-belief db belief)]
          :when (not= :like (:belief/feeling existing))]
      (assoc belief
             :db/id (:db/id existing) ; or -1
             :belief/feeling :like
             :belief/cause e1
             :belief/phrase
             (replacem "FOO likes me! We can be friends."
                       {"FOO" (name x)})))))

(defn x-angry-me-response
  [db person mind]
  (let [ans (d/q '[:find ?x ?e1
                   :in $ ?person ?mind %
                   :where
                   (feels-for ?e1 ?person ?mind ?x ?mind :anger)
                   ]
                 db person mind dbrules)]
    (for [[x e1] ans
          :let [belief {:belief/person person
                        :belief/mind mind
                        :belief/subject mind
                        :belief/object x}
                existing (existing-belief db belief)]
          :when (not (contains? #{:anger :fear} (:belief/feeling existing)))
          :let [new-feeling (rand-nth [:anger :fear])]]
      (assoc belief
             :db/id (:db/id existing)
             :belief/feeling new-feeling
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
  [db person mind]
  (let [ans (d/q '[:find ?x ?other ?e2
                   :in $ ?person ?mind %
                   :where
                   (feels-for ?e1 ?person ?mind ?mind ?x :like)
                   (feels-for ?e2 ?person ?mind ?x ?other :like)
                   [(not= ?other ?mind)]
                   ]
                 db person mind dbrules)]
    (for [[x other e2] ans
          :let [belief {:belief/person person
                        :belief/mind mind
                        :belief/subject mind
                        :belief/object other}
                existing (existing-belief db belief)]
          :when (not (contains? #{:anger :like} (:belief/feeling existing)))
          :let [new-feeling (rand-nth [:anger :like])]]
      (assoc belief
             :db/id (:db/id existing)
             :belief/feeling new-feeling
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
  [db person mind]
  )

(defn me-like-x-&-y-angry-x-response
  [db person mind]
  )

(defn me-fear-x-&-y-angry-x-response
  [db person mind]
  )

(defn me-angry-x-&-y-angry-x-response
  [db person mind]
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
  [db person mind]
  (mapcat (fn [response-fn]
            (response-fn db person mind))
          all-response-fns))

(defn think
  "Returns [db thoughts], where thoughts are any belief maps just
  added to the database."
  [db person]
  (let [thoughts (derive-beliefs db person person)]
    [(if (seq thoughts)
       (d/db-with db thoughts)
       db)
     thoughts]))

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
  (let [subj (:belief/subject belief)
        obj (:belief/object belief)
        feel (:belief/feeling belief)
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

(defn phrase-gossip
  "Returns string"
  [db speaker listener belief]
  (let [;; TODO: what if belief subject is the listener? a confrontation.
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
                          (= source (:belief/subject belief))
                          (replacem "FOO told me HIM/HERself."
                                    {"FOO" (name source)
                                     "HIM/HER" (him-her db source)})
                          :else
                          (format "I heard it from %s."
                                  (name source)))
                        ;; there is neither a cause nor a source
                        ""))
        ]
    (str/join \newline [message-prefix message-str explain-str])))

(defn all-people
  [db]
  (d/q '[:find [?person ...]
         :where
         [_ :person/id ?person]]
       db))

(defn random-partner
  [db initiator]
  (let [people (all-people db)]
    (rand-nth (remove #(= initiator %) people))))

(defn favourable-ness
  "How might it affect my popularity if others learn this belief?
  One of -1, 0, 1.

  A more sophisticated version would look at our theory of the
  listener's mind, and prioritise beliefs that (we think) would change
  their mind - particularly if would make me the most popular."
  [me belief]
  (let [subj (:belief/subject belief)
        obj (:belief/object belief)
        feeling (:belief/feeling belief)]
    (match [subj obj feeling]
           ;; i am liked
           [_ me :like] 1
           ;; i am not liked
           [_ me _] -1
           ;; someone else is liked
           [_ _ :like] -1
           ;; someone else is not liked
           [_ _ _] 1)))

(defn make-up-lies
  [db speaker listener]
  (let [people (all-people db)
        others (-> (set people)
                   (disj speaker listener)
                   (seq))]
    (->>
     (repeatedly
      100
      (fn []
        (let [other (rand-nth others)
              poss (if (> (count others) 1)
                     [:them :you :me]
                     [:you :me])]
          (case (rand-nth poss)
            :me [other speaker :like]
            :you [other listener :anger]
            :them (let [other2 (rand-nth (remove #(= % other) others))]
                    [other other2 :fear])))))
     (map #(zipmap [:belief/subject :belief/object :belief/feeling] %)))))

(defn indebted
  [db from to]
  (d/q '[:find ?e .
         :in $ ?from ?to
         :where
         [?e :debt/from ?from]
         [?e :debt/to ?to]]
       db from to))

(defn indebted-to
  [db from]
  (d/q '[:find [?to ...]
         :in $ ?from
         :where
         [?e :debt/from ?from]
         [?e :debt/to ?to]]
       db from))

(defn prioritised-potential-gossip
  [db speaker listener]
  (let [all-bs (->> (d/q my-beliefs-q db speaker)
                    (remove #(and (= speaker (:belief/subject %))
                                  (= listener (:belief/object %))))
                    (sort-by favourable-ness >))
        owing (indebted db speaker listener)]
    (concat all-bs
            ;; do we tell negative truths or positive lies...
            (when owing
              (make-up-lies db speaker listener)))))

(defn goss-result
  "
  * Listener checks whether
  - they already know it
    - they hold a more recent belief (new attr :time, can't use tx sinced copied)
  - it is a lie, which they can check if they are the subject
  * Listener responds with any corrections
  - including that they are angry with the source of a lie.
  * Listener updates their own beliefs according to corrected version
  * Listener updates model of speaker mind according to corrected version
  * Speaker updates model of listener mind with corrected version

  Returns keys
  :db
  :news?
  :lie?
  :minor-news?
  "
  [db speaker listener belief]
  (let [belief (assoc belief :source speaker) ;; TODO: cause nil?
        subj (:belief/subject belief)
        lis-facts {:my-mind (assoc belief
                                   :belief/person listener
                                   :belief/mind listener)
                   :spe-mind (assoc belief
                                    :belief/person listener
                                    :belief/mind speaker)
                   :subj-mind (assoc belief
                                     :belief/person listener
                                     :belief/mind subj)}
        spe-facts {:lis-mind (assoc belief
                                    :belief/person speaker
                                    :belief/mind listener)}
        knew-that? (fn [mind-belief]
                     (= (:belief/feeling (existing-belief db mind-belief))
                        (:belief/feeling belief)))
        ;; TODO: outdated / lies - corrections and responses
        to-apply (concat
                  (when-not (knew-that? (:my-mind lis-facts))
                    [(updatable-belief db (:my-mind lis-facts))
                     (updatable-belief db (:subj-mind lis-facts))])
                  (when-not (knew-that? (:spe-mind lis-facts))
                    [(updatable-belief db (:spe-mind lis-facts))])
                  (when-not (knew-that? (:lis-mind spe-facts))
                    [(updatable-belief db (:lis-mind spe-facts))]))
        ]
    {:db (if (seq to-apply) (d/db-with db to-apply) db)
     :news? (not (knew-that? (:my-mind lis-facts)))
     :minor-news? (not (knew-that? (:spe-mind lis-facts)))}
    )
  )

(defn turn-part
  "Returns keys

  :db ?
  :speaker
  :listener
  :new-beliefs
  :gossip (the belief new to the listener)
  :spe-thoughts ?
  :lis-thoughts ?
  :narrative
  "
  [db speaker listener]
  (let [owing (indebted db speaker listener)
        ]
    (loop [all-bs (prioritised-potential-gossip db speaker listener)
           minor-news []
           db db]
      (if-let [belief (first all-bs)]
        (let [result (goss-result db speaker listener belief)
              ]
          (if (:news? result)
            ;; found substantial gossip, so stop telling, clear debt
            (let [db (cond-> (:db result)
                       owing (d/db-with [[:db.fn/retractEntity owing]]))]
              {:db db
               :speaker speaker
               :listener listener
               :gossip belief
               :minor-news minor-news})
            ;; not news, keep trying
            (recur (rest all-bs)
                   (if (:minor-news? result)
                     (conj minor-news belief)
                     minor-news)
                   (:db result))))
        ;; tried everything, no gossip
        ;; fall in to debt
        (let [db (cond-> db
                   (not owing) (d/db-with [{:debt/from speaker
                                            :debt/to listener}]))]
          {:db db
           :speaker speaker
           :listener listener
           :gossip nil
           :minor-news minor-news})))))

(defn turn
  "Person has a turn being the initiator of an encounter.

g  Play proceeds as follows:

  * Initiator chooses a partner. (currently random)
  * Forward part; initiator is speaker, partner is listener.
  * Speaker chooses beliefs (possibly lies) to tell.
    - TODO: choose based on own model of their mind?
      - so listener can learn what we know
      - BUT would never pass on lies to subject because assume they know it.
      - what if we just pass on all our own beliefs? (excluding direct feelings)
        - could choose to add lies (or omit some beliefs?)
          - prioritize favourable info:
          - someone likes me, someone angry/fear others
      - keep telling until we come up with new gossip
        - ! if no new gossip, fall into debt with the listener / otherwise clear.
        - ! if were already in debt, we are forced to tell a lie!
  * Listener checks whether
    - they already know it
    - they hold a more recent belief
    - it is a lie, which they can check if they are the subject
  * Listener responds with any corrections
    - including that they are angry with the source of a lie.
  * Listener updates their own beliefs according to corrected version
  * Listener updates model of speaker mind according to corrected version
  * Speaker updates model of listener mind with corrected version
  * Speaker thinks.
  * Listener thinks.
  * Backward part; initiator is listener, partner is speaker.

  Many beliefs are communicated, and many will be already known by the
  listener. In narrative, maybe list the ones that are changing the
  speaker's model of listener, each like ''Yeah, I know.''
  The headline gossip is the new belief held by the listener. (if any)

  Outdated info does not count as gossip, but a lie that is uncovered does.

  Returns keys
  :db
  :initiator
  :partner
  :fwd-part
  :back-part
  :meet-phrase
  "
  [db initiator]
  (let [partner (random-partner db initiator)
        fwd-part (turn-part db initiator partner)
        db (:db fwd-part)
        [db partner-thoughts1] (think db partner)
        back-part (turn-part db partner initiator)
        db (:db back-part)
        [db initiat-thoughts1] (think db initiator)
        [db partner-thoughts2] (think db partner)
        ;; there might be more thoughts that can derived now:
        [db initiat-thoughts2] (if initiat-thoughts1
                                 (think db initiator)
                                 [db nil])
        [db partner-thoughts3] (if partner-thoughts2
                                 (think db partner)
                                 [db nil])
        ;; ok let's not go crazy... stop now.
        meet-str (-> (rand-nth meeting-phrases)
                     (replacem {"SPE" (name initiator)
                                "LIS" (name partner)
                                "HIM/HER" (him-her db partner)}))
        fwd-reply (if (contains? #{:anger :fear}
                                 (:belief/feeling (first partner-thoughts1)))
                    (rand-nth negative-response-phrases)
                    (rand-nth positive-response-phrases))
        back-reply (if (contains? #{:anger :fear}
                                  (:belief/feeling (first initiat-thoughts1)))
                    (rand-nth negative-response-phrases)
                    (rand-nth positive-response-phrases))

        ]
    {:db db
     :fwd-part fwd-part
     :back-part back-part
     :initiator initiator
     :partner partner
     :fwd-thoughts partner-thoughts1
     :back-thoughts initiat-thoughts1
     :initiator-thoughts initiat-thoughts2
     :partner-thoughts (concat partner-thoughts2
                               partner-thoughts3)
     ;; should be separate narrative function:
     :meet-phrase meet-str
     :fwd-reply fwd-reply
     :back-reply back-reply
     }))

(defn random-turn
  [db]
  (let [people (all-people db)
        initiator (rand-nth people)]
    (turn db initiator)))
