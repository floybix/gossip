(ns gossip.narrative
  (:require [gossip.core :as gossip :refer [replacem he-she him-her]]
            [datascript.core :as d]
            [clojure.set :as set]
            [clojure.string :as str]
            #?(:clj [clojure.core.match :refer [match]]
                    :cljs [cljs.core.match :refer-macros [match]])
            #?(:cljs [goog.string :as gstring :refer [format]])
            #?(:cljs [goog.string.format])))

(def meeting-phrases
  ["At a party that night, SPE walks up to LIS and talks to HIM/HER."
   "The next day, SPE sees LIS at the park."
   "In maths class, SPE passes a note to LIS."])

(def message-prefixes
  ["Did you know?"
   "I wanted to tell you,"
   "OMG I can't believe it!"])

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

(defn phrase-gossip
  "Returns string"
  [db speaker listener belief]
  (let [subj (:belief/subject belief)
        obj (:belief/object belief)
        message-prefix (rand-nth message-prefixes)
        message-str (str (if (= listener subj)
                           "I heard " "")
                         (phrase-belief db belief speaker listener))
        explain-str (if-let [cause-e (:belief/cause belief)]
                      ;; look up reason
                      (let [cause (d/pull db '[*] (:db/id cause-e))]
                        (when (:belief/feeling cause)
                          (str "It all started because "
                               (phrase-belief db cause speaker listener))))
                      ;; no or unknown/outdated cause
                      (cond
                        (= speaker subj)
                        "" ;; i just feel that way
                        (and (= speaker obj) (= :like (:belief/feeling belief)))
                        "" ;; of course they like me, who wouldn't
                        :else
                        "I don't know why."))
        source-str (if-let [source (:belief/source belief)]
                     (cond
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

(defn my-emotional-setting
  "Returns
  [i-feel they-feel emo-string]
  where i-feel is the what the initiator feels for partner,
  they-feel is what the initiator thinks partner feels for them,
  emo-string is a string describing the initiator's beliefs."
  [db initiator partner]
  (let [stance-q '[:find (pull ?e [*]) .
                   :in $ ?mind ?x %
                   :where
                   (feels-for ?e ?mind ?mind ?mind ?x _)]
        versus-q '[:find (pull ?e [*]) .
                   :in $ ?mind ?x %
                   :where
                   (feels-for ?e ?mind ?mind ?x ?mind _)]
        stance (d/q stance-q db initiator partner gossip/dbrules)
        versus (d/q versus-q db initiator partner gossip/dbrules)
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
  [db initiator partner]
  (let [[ini-feel ini-vs ini-str] (my-emotional-setting db initiator partner)
        [par-feel par-vs par-str] (my-emotional-setting db partner initiator)
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
            "Sadly, ")
          par-str))))

(defn narrate-turn
  [{:keys [db fwd-part back-part fwd-thoughts back-thoughts
           initiator partner]
    :as turn}]
  (let [
        meet-str (-> (rand-nth meeting-phrases)
                     (replacem {"SPE" (name initiator)
                                "LIS" (name partner)
                                "HIM/HER" (him-her db partner)}))
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
