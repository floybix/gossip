(ns gossip.core
  (:gen-class)
  (:require [clojure.set :as set]
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
   :belief/thought {:db/doc "An english phrase expressing this belief/feeling."
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
  simulation. Give a map of keywords to names (strings)."
  [character-map]
  (for [[k name] character-map]
    {:person/id k
     :person/name name}))

(def feelings-q
  "Query to pull out all relationship belief entities, parameterised by mind."
  '[:find (pull ?e [*])
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
        (let [new-belief (assoc belief :belief/mind listener)
              their-existing-belief (existing-relation conn new-belief)
              ]
          (if (= (:relation/feeling their-existing-belief)
                 (:relation/feeling new-belief))
            ;; already known, continue
            (recur (rest bs))
            ;; got one
            new-belief))
        ;; checked all, no new gossip
        nil))))

(defn updatable-relation
  [conn new-belief]
  (let [existing (existing-relation conn new-belief)
        eid (or (:db/id existing) -1)]
    (assoc new-belief :db/id eid)))

(defn believe!
  [conn mind subject object feeling]
  (let [belief {:belief/mind mind
                :relation/subject subject
                :relation/object object
                :relation/feeling feeling}
        em (updatable-relation conn belief)]
    (d/transact! conn [em])
    em))

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
       :belief/source mind
       :belief/thought
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
             :belief/source mind
             :belief/thought
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
             :belief/source mind
             :belief/thought
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
             :belief/source mind
             :belief/thought
             (cond
               (= :anger new-feeling)
               (format "Oh, why does %1$s like %2$s? I want %1$s to only like me. Now I'm angry with %2$s."
                       (name x) (name other))
               (= :like new-feeling)
               (format "Great, %1$s likes %2$s so they must be fun, we can all hang out together."
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

(comment
  (use 'clojure.pprint)
  (def conn (d/create-conn db-schema))
  (believe! conn :fred :fred :ethel :fear)
  (believe! conn :fred :ethel :fred :fear)
  (pprint (d/q feelings-q @conn :fred))
  (d/transact! conn (dont-fear-each-other conn :fred))
  (pprint (d/q feelings-q @conn :fred))
)


(comment

  (defn random-meeting
   [state]
   (let [chars (::characters state)
         speaker (rand-nth (seq chars))
         listener (rand-nth (seq (disj chars speaker)))]
     (meet state speaker listener))))



(defn -main
  [& args]
  (println "Hello, World!"))
