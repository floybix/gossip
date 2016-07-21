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
        message-str (str (if (= listener (:belief/subject belief))
                           "I heard " "")
                         (phrase-belief db belief speaker listener))
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
