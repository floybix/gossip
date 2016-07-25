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
               :db/unique :db.unique/identity}
   :person/gender {:db/doc "A character's sex, :female or :male"}
   :debt/from {:db/doc "Who owes the debt."
               :db/index true}
   :debt/to {:db/doc "Who the debt is owed to."}
   :belief/person {:db/doc "Who holds this belief."
                   :db/index true}
   :belief/mind {:db/doc "Who believes this? I.e. person believes that
                          mind thinks subject likes object."
                 :db/index true}
   :belief/cause {:db/doc "Which other belief led to this belief."
                  :db/valueType :db.type/ref}
   :belief/complex-cause {:db/doc "Data describing a more complex cause."}
   :belief/source {:db/doc "Who did I learn this from."}
   :belief/lie? {:db/doc "True if this is a lie (unbeknownst to the
                          holder). This is stored as a convenience."}
   :belief/fabricator {:db/doc "Who was the original source of a lie."}
   :belief/subject {:db/doc "Who feels the feeling, eg the angry one."}
   :belief/object {:db/doc "Who is the feeling felt for, eg the one angry with."}
   :belief/feeling {:db/doc "What is the feeling, eg anger."}
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

(def my-true-popularity-q
  "Query to find how popular a character is (how many people like
  them). Parameter is person id, the one liked by ?n others."
  '[:find (count ?mind) .
    :in $ ?obj
    :where
    [?e :belief/person ?mind]
    [?e :belief/mind ?mind]
    [?e :belief/subject ?mind]
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

(defn replacement-beliefs
  [db new-beliefs]
  (->> new-beliefs
       (mapcat (fn [belief]
                 (let [existing (existing-belief db belief)]
                   (if-let [eid (:db/id existing)]
                     (if (= (:belief/feeling existing)
                            (:belief/feeling belief))
                       ;; no change necessary!
                       []
                       ;; need new belief; retract the old belief first.
                       [[:db.fn/retractEntity eid]
                        (dissoc belief :db/id)])
                     ;; no existing record
                     [(dissoc belief :db/id)]))))))

(defn believe
  ([db person mind subject object feeling]
   (let [belief {:belief/person person
                 :belief/mind mind
                 :belief/subject subject
                 :belief/object object
                 :belief/feeling feeling}]
     (believe db belief)))
  ([db belief]
   (d/db-with db (replacement-beliefs db [belief]))))

(defn replacem
  [s replacements]
  (reduce (fn [s [from to]]
            (str/replace s from to))
          s replacements))

(defn he-she
  ([db person he she]
   (case (-> (d/pull db [:person/gender] [:person/id person])
             :person/gender)
     :male he
     :female she))
  ([db person]
   (he-she db person "he" "she")))

(defn him-her
  [db person]
  (he-she db person "him" "her"))

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
      {:belief/person person
       :belief/mind mind
       :belief/subject mind
       :belief/object x
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
             :belief/feeling new-feeling
             :belief/cause e1
             :belief/phrase
             (cond
               (= :anger new-feeling)
               (replacem "Well if FOO is angry with me then I'm angry with HIM/HER."
                         {"FOO" (name x)
                          "HIM/HER" (him-her db x)})
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
             :belief/feeling new-feeling
             :belief/cause e2
             :belief/phrase
             (cond
               (= :anger new-feeling)
               (replacem "Oh, why does SUBJ like OBJ? I want SUBJ to
                             only like me. Now I'm angry with OBJ."
                         {"SUBJ" (name x), "OBJ" (name other)})
               (= :like new-feeling)
               (replacem "Great, SUBJ likes OBJ so HE/SHE must be fun,
                             I'll be friends with OBJ too."
                         {"SUBJ" (name x), "OBJ" (name other)
                          "HE/SHE" (he-she db other)}))))))

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
  (let [ans (d/q '[:find ?x ?other ?e2
                   :in $ ?person ?mind %
                   :where
                   (feels-for ?e1 ?person ?mind ?mind ?x :anger)
                   (feels-for ?e2 ?person ?mind ?other ?x :anger)
                   [(not= ?other ?mind)]
                   ]
                 db person mind dbrules)]
    (for [[x other e2] ans
          ;; nothing to do if i already like them
          :let [belief {:belief/person person
                        :belief/mind mind
                        :belief/subject mind
                        :belief/object other}
                existing (existing-belief db belief)]
          :when (not= :like (:belief/feeling existing))
          ]
      (assoc belief
             :belief/feeling :like
             :belief/cause e2
             :belief/phrase
             (replacem "OBJ and I are both angry with SUBJ. Yeah, we're on the same team.
                       I like HIM/HER."
                       {"SUBJ" (name x), "OBJ" (name other)
                        "HIM/HER" (him-her db other)})))))

(defn like-more-popular-non-enemies
  [db person mind]
  (let [likes (->> (d/q perceived-popularity-q
                        db person mind)
                   (into {}))
        my-n-likes (get likes mind)
        ]
    (for [[x n-likes] (dissoc likes mind)
          ;; those (much) more popular than me
          :when (>= n-likes (+ my-n-likes 2))
          ;; nothing to do if i already like them
          :let [belief {:belief/person person
                        :belief/mind mind
                        :belief/subject mind
                        :belief/object x}
                existing (existing-belief db belief)]
          :when (not= :like (:belief/feeling existing))
          ;; skip if anyone i like is angry with them
          :let [ans (d/q '[:find ?friend
                           :in $ ?person ?mind ?x %
                           :where
                           (feels-for ?e1 ?person ?mind ?mind ?friend :like)
                           (feels-for ?e2 ?person ?mind ?friend ?x :anger)
                           ]
                         db person mind x dbrules)]
          ]
      (assoc belief
             :belief/feeling :like
             :belief/complex-cause :popularity
             :belief/phrase
             (replacem "FOO is so cool, HE/SHE already has NUMB friends. I like HIM/HER."
                       {"FOO" (name x)
                        "HE/SHE" (he-she db x)
                        "NUMB" (likes x)
                        "HIM/HER" (him-her db x)})
             ))))

(def all-response-fns
  [mutual-fear-response
   x-like-me-response
   x-angry-me-response
   me-like-x-&-x-like-y-response
   me-angry-x-&-y-angry-x-response
   like-more-popular-non-enemies])

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
       ;;(d/db-with db (replacement-beliefs db thoughts))
       ;; avoid duplicate beliefs - replace one at a time
       (reduce (fn [db b]
                 (d/db-with db (replacement-beliefs db [b])))
               db thoughts)
       db)
     thoughts]))

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
  A number where negative is bad and positive is good.

  A more sophisticated version would look at our theory of the
  listener's mind, and prioritise beliefs that (we think) would change
  their mind - particularly if would make me the most popular."
  [me belief]
  (let [subj (:belief/subject belief)
        obj (:belief/object belief)
        feeling (:belief/feeling belief)]
    (match [subj obj feeling]
           ;; i am liked
           [_ me :like] 2
           ;; i am not liked
           [_ me _] -2
           ;; someone else is liked
           [_ _ :like] -1
           ;; someone else is not liked
           [_ _ _] 1)))

(defn make-up-lies
  [db speaker listener]
  (let [people (all-people db)
        others (remove #{speaker listener} people)]
    (->>
     (repeatedly
      100
      (fn []
        (let [other (rand-nth others)
              poss (if (> (count others) 1)
                     [:them :you :me :me]
                     [:you :me :me])]
          (case (rand-nth poss)
            :me [other speaker :like]
            :you [other listener :anger]
            :them (let [other2 (rand-nth (remove #(= % other) others))]
                    [other other2 :fear])))))
     (map #(zipmap [:belief/subject :belief/object :belief/feeling] %))
     (map #(assoc % :belief/lie? true, :belief/fabricator speaker)))))

(defn indebted
  [db from to]
  (d/q '[:find ?e .
         :in $ ?from ?to
         :where
         [?e :debt/from ?from]
         [?e :debt/to ?to]]
       db from to))

(def indebted-to-q
  '[:find [?to ...]
    :in $ ?from
    :where
    [?e :debt/from ?from]
    [?e :debt/to ?to]])

(defn update-debt
  [db from to had-gossip?]
  (let [owing (indebted db from to)]
    (cond
      ;; clear debt
      (and owing had-gossip?)
      (d/db-with db [[:db.fn/retractEntity owing]])
      ;; fall in to debt
      (and (not owing) (not had-gossip?))
      (d/db-with db [{:debt/from from
                      :debt/to to}])
      :else
      db)))

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

  There are three cases
  - listener knows it was a lie, and responds with corrected belief and their new anger.
  - listener knows it was outdated, and responds with corrected belief.
  - otherwise it is treated as correct, but may or may not be news.

  Returns keys
  :db
  :existing
  :wrong?
  :known-lie?
  :news?
  :minor-news?
  "
  [db speaker listener belief]
  (let [source (:belief/source belief)
        belief (assoc belief :belief/source speaker)
        subj (:belief/subject belief)
        lis-fact (assoc belief
                        :belief/person listener
                        :belief/mind listener)
        ;; always learn that speaker's source knows it too.
        db (if (and source (not= source listener))
             (let [to-apply [(assoc belief :belief/person listener :belief/mind source)]]
               (d/db-with db (replacement-beliefs db to-apply)))
             db)
        knew-that? (fn [mind-belief]
                     (= (:belief/feeling (existing-belief db mind-belief) :none)
                        (:belief/feeling belief :none)))
        existing (existing-belief db lis-fact)
        ;; do not believe or correct your own lies!
        secret-lie? (= (:belief/fabricator belief) listener)
        news? (and (not= (:belief/feeling existing :none)
                         (:belief/feeling belief :none))
                   (not secret-lie?))
        correction? (and (= subj listener) news?)
        ;; TODO: exposed lie does not count as gossip if we already know about it
        ;; also - knowledge of the lie should pass back to speaker
        exposed-lie? (and correction? (:belief/lie? belief))]
    (if correction?
      (let [corrected (or existing
                          {:belief/person listener
                           :belief/mind listener
                           :belief/subject (:belief/subject belief)
                           :belief/object (:belief/object belief)
                           :belief/feeling :none})
            fixed (goss-result db listener speaker corrected)
            db (:db fixed)]
        (if exposed-lie?
         (let [from source ; or (:belief/fabricator belief)
               angry {:belief/person listener
                      :belief/mind listener
                      :belief/subject listener
                      :belief/object from
                      :belief/feeling :anger
                      :belief/complex-cause :lied-about-me}
               ;; if we thought they liked us before, now assume they don't.
               ;; this prevents listener from reciprocally re-liking fabricator...
               ;; TODO: need this?? might be detrimental to listener.
               nolike {:belief/person listener
                       :belief/mind listener
                       :belief/subject from
                       :belief/object listener
                       :belief/feeling :none
                       :belief/complex-cause :its-unfriendly-to-lie}
               waslike? (= :like (:belief/feeling (existing-belief db nolike)))
               to-apply (for [belief (if waslike? [angry nolike] [angry])
                              person [speaker listener]
                              mind [speaker listener]]
                          (assoc belief :belief/person person :belief/mind mind))
               ]
           {:db (d/db-with db (replacement-beliefs db to-apply))
            :existing existing
            :wrong? true
            :exposed-lie? true
            :reaction angry
            :news? true})
         ;; wrong, but not a lie - i.e. outdated; news to speaker
         (assoc fixed
                :existing existing
                :wrong? true
                :news? false)))
      ;; usual case - no need for correction
      (let [to-apply (->>
                      [(when-not secret-lie?
                         (assoc belief :belief/person listener :belief/mind listener))
                       (when-not secret-lie?
                         (when (and (not= subj speaker) (not= subj listener))
                           (assoc belief :belief/person listener :belief/mind subj)))
                       (assoc belief :belief/person listener :belief/mind speaker)
                       (assoc belief :belief/person speaker :belief/mind listener)
                       ]
                      (remove nil?))]
        {:db (d/db-with db (replacement-beliefs db to-apply))
         :existing existing
         :news? news?
         :minor-news? (not (knew-that? (assoc lis-fact :belief/mind speaker)))}))
    ))

(defn turn-part
  "Returns keys

  :db
  :db-before
  :speaker
  :listener
  :gossip (the belief new to the listener)
  :existing (the belief the listener held before)
  :exposed-lie?
  :reaction
  :back-gossip (in case there were corrections - beliefs new to speaker)
  :minor-news
  "
  [db speaker listener potential-gossip]
  (let [db-before db
        partial-result {:db-before db
                        :speaker speaker
                        :listener listener}]
    (loop [all-bs potential-gossip
           back-gossip []
           minor-news []
           db db]
      (if-let [belief (first all-bs)]
        (let [response (goss-result db speaker listener belief)]
          (cond
            ;; exposing a lie is sufficient gossip to stop telling and clear debt.
            (:exposed-lie? response)
            (assoc partial-result
                   :db (:db response)
                   :gossip belief
                   :reaction (:reaction response)
                   :existing (:existing response)
                   :exposed-lie? true
                   :back-gossip back-gossip
                   :minor-news minor-news)
            ;; outdated info does not count as gossip.
            (:wrong? response)
            (recur (rest all-bs)
                   (conj back-gossip [belief (:existing response)])
                   minor-news
                   (:db response))
            ;; valid gossip
            (:news? response)
            (assoc partial-result
                   :db (:db response)
                   :gossip belief
                   :existing (:existing response)
                   :back-gossip back-gossip
                   :minor-news minor-news)
            ;; not news, keep trying
            :else
            (recur (rest all-bs)
                   back-gossip
                   (if (:minor-news? response)
                     (conj minor-news belief)
                     minor-news)
                   (:db response))))
        ;; tried everything, no gossip
        (assoc partial-result
               :db db
               :gossip nil
               :back-gossip back-gossip
               :minor-news minor-news)))))

(defn continue-turn
  [db initiator partner fwd-part]
  (let [;; listener learns cause too (another belief)
        ;; TODO: hide cause if it is a direct feeling / or own lie
        fwd-cause-part (when-let [cause-ref (:belief/cause (:gossip fwd-part))]
                         (let [cause (d/pull db '[*] (:db/id cause-ref))]
                           (println "fwd cause " cause)
                           (turn-part db initiator partner
                                      [(dissoc cause :belief/source :belief/cause)])))
        db (if fwd-cause-part (:db fwd-cause-part) db)
        ;; partner speaks (think first)
        [db partner-thoughts1] (think db partner)
        back-part (turn-part db partner initiator
                             (prioritised-potential-gossip db partner initiator))
        db (:db back-part)
        ;; listener learns cause too (another belief)
        back-cause-part (when-let [cause-ref (:belief/cause (:gossip back-part))]
                         (let [cause (d/pull db '[*] (:db/id cause-ref))]
                           (println "back cause " cause-ref)
                           (turn-part db partner initiator
                                      [(dissoc cause :belief/source :belief/cause)])))
        db (if back-cause-part (:db back-cause-part) db)
        ;; update debts
        db (update-debt db initiator partner (or (:gossip fwd-part)
                                                 (seq (:back-gossip back-part))))
        db (update-debt db partner initiator (or (:gossip back-part)
                                                 (seq (:back-gossip fwd-part))))
        ]
    {:db db
     :fwd-part fwd-part
     :back-part back-part
     :fwd-cause-part fwd-cause-part
     :back-cause-part back-cause-part
     :initiator initiator
     :partner partner
     :fwd-thoughts partner-thoughts1
     }))

(defn turn*
  "Person has a turn being the initiator of an encounter.

  Play proceeds as follows:

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
  "
  [db initiator partner]
  (let [;; initiator speaks
        fwd-part (turn-part db initiator partner
                            (prioritised-potential-gossip db initiator partner))
        db (:db fwd-part)
        ;; if had back-gossip, clear partner debt before next part
        db (if (or (seq (:back-gossip fwd-part))
                   (:exposed-lie? fwd-part))
             (update-debt db partner initiator true)
             db)
        ]
    (continue-turn db initiator partner fwd-part)))

(defn initiator-think
  [turn]
  (let [{:keys [db initiator]} turn
        ;; do some thinking
        [db initiat-thoughts1] (think db initiator)
        ;; there might be more thoughts that can derived now:
        [db initiat-thoughts2] (if initiat-thoughts1
                                 (think db initiator)
                                 [db nil])
        ;; ok let's not go crazy... stop now.
        ]
    (assoc turn
           :db db
           :back-thoughts initiat-thoughts1
           :initiator-thoughts initiat-thoughts2)))

(defn partner-think
  [turn]
  (let [{:keys [db partner]} turn
        partner-thoughts1 (:fwd-thoughts turn)
        ;; do some thinking
        [db partner-thoughts2] (think db partner)
        ;; there might be more thoughts that can derived now:
        [db partner-thoughts3] (if partner-thoughts2
                                 (think db partner)
                                 [db nil])
        ;; ok let's not go crazy... stop now.
        ]
    (assoc turn
           :db db
           :fwd-thoughts partner-thoughts1
           :partner-thoughts (concat partner-thoughts2
                                     partner-thoughts3))))

(defn turn-think
  [turn]
  (-> turn
      (partner-think)
      (initiator-think)))

(defn turn
  [db initiator partner]
  (-> (turn* db initiator partner)
      (turn-think)))

(defn random-turn
  [db]
  (let [people (all-people db)
        initiator (rand-nth people)
        partner (random-partner db initiator)]
    (turn db initiator partner)))
