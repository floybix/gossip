(ns gossip.narrative
  (:require [gossip.core :as gossip :refer [replacem he-she him-her]]
            [datascript.core :as d]
            [clojure.set :as set]
            [clojure.string :as str]
            #?(:clj [clojure.core.match :refer [match]]
                    :cljs [cljs.core.match :refer-macros [match]])
            #?(:cljs [goog.string :as gstring :refer [format]])
            #?(:cljs [goog.string.format])))

(defn meeting-phrase
  [db initiator partner]
  (-> ["At a party that night, SPE walks up to LIS and talks to HIM/HER."
       "The next day, SPE sees LIS at the park."
       "In english class, SPE passes a note to LIS."]
      (rand-nth)
      (replacem {"SPE" (name initiator)
                 "LIS" (name partner)
                 "HIM/HER" (him-her db partner)})))

(def message-prefixes
  ["Did you know?"
   "I wanted to tell you,"
   "OMG I can't believe it!"])

(def no-gossip-phrases
  ["I got nothing, sorry."])

(def no-gossip-response-phrases
  ["You owe me, ok?"])

(def lie-response-phrases
  ["I'm so angry with SOURCE for spreading lies about me!"])

(def correction-phrases
  ["No, that's wrong. Now CORRECT"])

(def correction-response-phrases
  ["Really? So it's not true that OLDBELIEF"])

(def embarrassed-phrases
  [[:span [:i "Gulp."] " Er, gotta go now, bye!"]])

(def minor-news-response-phrases
  ["Oh yeah. I was going to tell you..."
   "I heard that too."])

(def already-knew-response-phrases
  ["Yeah yeah, I know."])

(def positive-response-phrases
  ["Ha ha."
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
      (not feel) ""
      ;; my feeling
      (= subj speaker) ;; quite different grammar...
      (case feel
        :like (format "I like %s." obj-name)
        :anger (format "I'm angry with %s." obj-name)
        :fear (format "I'm afraid of %s." obj-name)
        :none (format "I don't care about %s." obj-name))
      ;; your feeling
      (= subj listener)
      (case feel
        :like (format "you like %s." obj-name)
        :anger (format "you are angry with %s." obj-name)
        :fear (format "you are afraid of %s." obj-name)
        :none (format "you don't care about %s." obj-name))
      ;; someone else's feeling
      :else
      (case feel
        :like (format "%s likes %s." (name subj) obj-name)
        :anger (format "%s is angry with %s." (name subj) obj-name)
        :fear (format "%s is afraid of %s." (name subj) obj-name)
        :none (format "%s doesn't care about %s." (name subj) obj-name))
      )
    ))

(defn belief-explanation
  [db pov belief]
  (let [cause-ref (:belief/cause belief)
        source (:belief/source belief)
        expl (if cause-ref
               (let [cause (d/pull db '[*] (:db/id cause-ref))]
                 (str "Because "
                      (phrase-belief db cause nil nil)
                      " "))
               "I don't know why. ")
        ]
    (cond->
        expl
      source
      (str "Heard it from " (name source) "."))))

(defn phrase-gossip
  "Returns string"
  [db speaker listener belief]
  (let [subj (:belief/subject belief)
        obj (:belief/object belief)
        source (:belief/source belief)
        prefix* (rand-nth message-prefixes)
        message-prefix (cond
                         (= listener subj source)
                         (str "You know how you told me")
                         (= listener subj)
                         (str prefix* " I heard")
                         :else
                         prefix*)
        message-str (phrase-belief db belief speaker listener)
        explain-str (cond
                      ;; a simple cause
                      (:belief/cause belief)
                      (let [cause-ref (:belief/cause belief)
                            cause (d/pull db '[*] (:db/id cause-ref))]
                        (when (:belief/feeling cause)
                          (str "It all started because "
                               (phrase-belief db cause speaker listener))))
                      ;; a complex cause
                      (:belief/complex-cause belief)
                      (let [cc (:belief/complex-cause belief)]
                        (cond
                          (= cc :lied-about-me)
                          (replacem "It's because OBJ spread lies about SUBJ."
                                    {"OBJ" (cond
                                             (= obj speaker) "I"
                                             (= obj listener) "you"
                                             :else (he-she db obj))
                                     "SUBJ" (cond
                                              (= subj speaker) "me"
                                              (= subj listener) "you"
                                              :else (name subj))})
                          (= cc :its-unfriendly-to-lie)
                          (replacem "OBJ knows that because SUBJ spread lies about THEM."
                                    {"OBJ" (cond
                                             (= obj speaker) "I"
                                             (= obj listener) "you"
                                             :else (name obj))
                                     "THEM" (cond
                                              (= obj speaker) "me"
                                              (= obj listener) "you"
                                              :else (him-her db obj))
                                     "SUBJ" (cond
                                              (= subj speaker) "I"
                                              (= subj listener) "you"
                                              :else (name subj))})
                          (= cc :popularity)
                          (replacem "It's because OBJIS so cool."
                                    {"OBJIS" (cond
                                               (= obj speaker) "they think I'm"
                                               (= obj listener) "you're"
                                               :else (he-she db obj))})
                          :else
                          (str "It's because " cc)))
                      ;; otherwise - no or unknown/outdated cause
                      (= speaker subj)
                      "" ;; i just feel that way
                      (and (= speaker obj) (= :like (:belief/feeling belief)))
                      "" ;; of course they like me, who wouldn't
                      (= listener subj)
                      "" ;; do you feel that way?
                      :else
                      "I don't know why.")
        source-str (if source
                     (cond
                       (= source listener)
                       "..."
                       (= source subj)
                       (replacem "FOO told me HIM/HERself."
                                 {"FOO" (name source)
                                  "HIM/HER" (him-her db source)})
                       :else
                       (format "I heard it from %s."
                               (name source)))
                     ;; there is no source
                     "")
        ]
    (str/join \newline [message-prefix message-str explain-str source-str])))

(defn phrase-response
  [db speaker listener gossip
   {:keys [news? wrong? exposed-lie? reaction existing minor-news?]
    :as response}]
  (cond
    ;; lie correction and reaction, if any
    exposed-lie?
    (let [source (:belief/object reaction)]
      (str "That's a lie!"
           (if (= :like (:belief/feeling gossip))
             (str " I don't like " (him-her db (:belief/object gossip)) ".")
             (when existing
               (str " " (phrase-belief db existing listener speaker))))
           " "
           (-> (rand-nth lie-response-phrases)
               (replacem {"SOURCE" (name source)}))))
    ;; correcting outdated
    wrong?
    (-> (rand-nth correction-phrases)
        (replacem {"CORRECT" (phrase-belief db existing listener speaker)}))
    ;; note existing belief that was replaced, if any
    (and news? existing)
    (-> (rand-nth correction-response-phrases)
        (replacem {"OLDBELIEF" (phrase-belief db existing listener speaker)}))
    ;; gossip
    news?
    (rand-nth positive-response-phrases)
    ;; listener didn't know that speaker knew that
    minor-news?
    (rand-nth minor-news-response-phrases)
    :else
    (rand-nth already-knew-response-phrases)
    ))

(defn my-emotional-setting
  "Returns
  [i-feel they-feel emo-string]
  where i-feel is the what the initiator feels for partner,
  they-feel is what the initiator thinks partner feels for them,
  emo-string is a string describing the initiator's beliefs."
  [db initiator partner pov]
  (let [stance-q '[:find (pull ?e [*]) .
                   :in $ ?pov ?mind ?x %
                   :where
                   (feels-for ?e ?pov ?mind ?mind ?x _)]
        versus-q '[:find (pull ?e [*]) .
                   :in $ ?pov ?mind ?x %
                   :where
                   (feels-for ?e ?pov ?mind ?x ?mind _)]
        stance (d/q stance-q db pov initiator partner gossip/dbrules)
        versus (d/q versus-q db pov initiator partner gossip/dbrules)
        no-stance {:belief/subject initiator, :belief/object partner :belief/feeling :none}
        no-versus {:belief/subject partner, :belief/object initiator :belief/feeling :none}
        vs (case [(:belief/feeling stance :none)
                  (:belief/feeling versus :none)]
             [:like :like] "and"
             [:fear :anger] "and"
             [:anger :fear] "and"
             [:anger :anger] "and"
             [:none :none] "and"
             ;; otherwise:
             "but")]
    [(:belief/feeling stance :none)
     (:belief/feeling versus :none)
     (str
      (phrase-belief db (or stance no-stance) nil nil)
      ".. " vs " thinks "
      (-> (phrase-belief db (or versus no-versus) nil nil)
          (replacem {(name initiator) (him-her db initiator)}))
      )]))

(defn emotional-setting
  [db initiator partner pov]
  (let [[ini-feel ini-vs ini-str] (my-emotional-setting db initiator partner
                                                        (or pov initiator))
        [par-feel par-vs par-str] (my-emotional-setting db partner initiator
                                                        (or pov partner))
        ini-corr? (= ini-vs par-feel)
        par-corr? (= par-vs ini-feel)
        ini (name initiator)
        par (name partner)
        rm {"INI" ini, "PAR" par}]
    (cond
      (= :none ini-feel ini-vs par-feel par-vs)
      ""
      (= :like ini-feel ini-vs par-feel par-vs)
      (replacem "INI and PAR are friends." rm)
      (= :anger ini-feel ini-vs par-feel par-vs)
      (replacem "INI and PAR are angry with each other and both know it." rm)
      ;; initiator alone
      (= :none ini-vs par-feel par-vs)
      (-> (str (case ini-feel
                 :like "INI likes PAR"
                 :anger "INI is angry with PAR"
                 :fear "INI is afraid of PAR")
               ", but PAR doesn't know it.")
          (replacem rm))
      ;; partner alone
      (= :none ini-feel ini-vs par-vs)
      (-> (str (case par-feel
                 :like "PAR likes INI"
                 :anger "PAR is angry with INI"
                 :fear "PAR is afraid of INI")
               ", but INI doesn't know it.")
          (replacem rm))
      :else ;; it's complicated
      (str ini-str " "
          (cond
            (and ini-corr? par-corr?)
            "Equally, "
            ini-corr?
            "It's true, "
            par-corr?
            "However, "
            :else ;; both wrong!
            "However, ")
          par-str))))

(defn narrate-turn
  [{:keys [db fwd-part back-part fwd-thoughts back-thoughts
           initiator partner]
    :as turn}]
  (let [
        meet-str (meeting-phrase db initiator partner)
        fwd-reply (when (:gossip fwd-part)
                    (if (contains? #{:anger :fear}
                                   (:belief/feeling (first fwd-thoughts)))
                      (rand-nth negative-response-phrases)
                      (rand-nth positive-response-phrases)))
        back-reply (when (:gossip back-part)
                     (if (contains? #{:anger :fear}
                                    (:belief/feeling (first back-thoughts)))
                       (rand-nth negative-response-phrases)
                       (rand-nth positive-response-phrases)))]
    (assoc turn
           :meet-phrase meet-str
           :fwd-reply fwd-reply
           :back-reply back-reply)
    ))
