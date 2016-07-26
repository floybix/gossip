(ns gossip.app
  (:require [reagent.core :as reagent :refer [atom]]
            [goog.dom.forms :as forms]
            [datascript.core :as d]
            [gossip.core :as gossip :refer [he-she him-her replacem]]
            [gossip.narrative :as narr]
            [clojure.set :as set]
            [clojure.string :as str]))

(enable-console-print!)

(defonce app-state
  (atom {:db (d/empty-db gossip/db-schema)
         :encounter nil
         :interactive nil
         :day 0
         }))

(defonce ui-state
  (atom {:current-pov nil
         :playing-as nil
         :choosing-avatar nil
         :adding-person {:name ""
                         :male? false}
         :adding-belief {:subject nil
                         :object nil
                         :feeling :like}
         :play-belief {:subject nil
                       :object nil
                       :feeling :like}}))

(defn init-interactive
  [db player partner]
  {:player player
   :partner partner
   :db db
   :meet-str (narr/meeting-phrase db player partner)
   :attempts []})

(defonce undo-buffer
  (atom ()))

(defonce redo-buffer
  (atom ()))

(defn swap-advance!
  "ref = app-state"
  [ref f & more]
  ;; record state for undo
  (swap! undo-buffer conj @ref)
  (when (seq @redo-buffer)
    (reset! redo-buffer ()))
  (apply swap! ref f more))

(def avatars
  (let [humans ["👦" "👧" "👨" "👩" "👱" "👲" "👳" "👴" "👵" "👶" "👸" "🙍" "🙎" "💁" "🙋" "💂" "🕵" "🎅" "👼" "👷" "👮" "👰" "🙇"]
        skins ["🏻" "🏼" "🏽" "🏾" "🏿"]
        others ["👹" "👺" "👻" "👽" "👿" "😺" "😼" "🐤" "🐥" "🐦" "🐧" "🐨" "🐬" "🐭" "🐮" "🐯" "🐰" "🐱" "🐲" "🐳" "🐴" "🐵" "🐶" "🐷" "🐸" "🐹" "🐺" "🐻" "🐼" "🐽" "💩"
]]
    (vec (concat humans
                 (for [skin skins
                       x humans]
                   (str x skin))
                 others))))

(defn add-person-pane
  [app-state ui-state]
  (let [person (:adding-person @ui-state)]
    [:div.form-inline.well
     [:div.form-group
      ;; name
      [:label "Name"]
      [:input.form-control
       {:type :text
        :value (:name person)
        :placeholder "Name"
        :on-change (fn [e]
                     (let [s (-> e .-target forms/getValue)]
                       (swap! ui-state assoc-in
                              [:adding-person :name]
                              s)))}]]
     ;; gender
     [:div.checkbox
      [:label
       [:input
        {:type :checkbox
         :checked (if (:male? person) true)
         :on-change (fn [_]
                      (swap! ui-state update-in
                             [:adding-person :male?]
                             not))}]
       "male"]]
     ;; add button
     [:button.btn.btn-default
      {:on-click
       (fn [_]
         (let [s (str/replace (:name person) " " "_")
               id (keyword s)
               gender (if (:male? person)
                        :male :female)
               avatar (rand-nth avatars)]
           (swap-advance! app-state update :db
                          d/db-with [{:person/id id
                                      :person/gender gender
                                      :person/avatar avatar}])
           (swap! ui-state assoc-in [:adding-person :name] "")))
       :disabled (when (str/blank? (:name person))
                   "disabled")}
      "Add person"]]))

(defn belief-input
  [ui-state ui-state-key db subj-disabled]
  (let [belief (get @ui-state ui-state-key)
        people (gossip/all-people db)]
    [:div.form-group
      ;; subject
      [:select.form-control
       {:value (or (:subject belief) " ")
        :on-change (fn [e]
                     (let [s (-> e .-target forms/getValue)]
                       (swap! ui-state assoc-in
                              [ui-state-key :subject]
                              (when-not (str/blank? s) (keyword s)))))
        :disabled (when subj-disabled "disabled")}
       (for [person (cons " " people)]
         [:option {:key person
                   :value person}
          (name person)])
       ]
      ;; feeling
      [:select.form-control
       {:value (:feeling belief)
        :on-change (fn [e]
                     (let [s (-> e .-target forms/getValue)]
                       (swap! ui-state assoc-in
                              [ui-state-key :feeling]
                              (keyword s))))}
       (for [feeling [:like :anger :fear :none]]
         [:option {:key feeling
                   :value feeling}
          (case feeling
            :like "likes"
            :anger "angry with"
            :fear "fears"
            :none "doesn't care about")])
       ]
      ;; object
      [:select.form-control
       {:value (or (:object belief) " ")
        :on-change (fn [e]
                     (let [s (-> e .-target forms/getValue)]
                       (swap! ui-state assoc-in
                              [ui-state-key :object]
                              (when-not (str/blank? s) (keyword s)))))}
       (for [person (cons " " people)]
         [:option {:key person
                   :value person}
          (name person)])
       ]]))

(defn add-belief-pane
  [app-state ui-state]
  (let [belief (:adding-belief @ui-state)
        db (:db @app-state)]
    [:div.form-inline.well
     (belief-input ui-state :adding-belief db (:playing-as @ui-state))
     ;; add button
     [:button.btn.btn-default
      {:on-click
       (fn [_]
         (let [{:keys [subject object feeling]} belief]
           (swap-advance! app-state update :db
                          gossip/believe subject subject subject object feeling)
           (swap! ui-state assoc-in [:adding-belief :object] nil)))
       :disabled (when (or (not (:subject belief))
                           (not (:object belief))
                           (= (:subject belief) (:object belief)))
                   "disabled")}
      "Add/replace feeling"]]))

(defn status-pane
  [app-state ui-state]
  (let [db (:db @app-state)
        playing? (:playing-as @ui-state)
        pov (or playing? (:current-pov @ui-state))
        people (gossip/all-people db)
        belief-li (fn [mind belief]
                    (let [b-mind (:belief/mind belief)
                          b-person (:belief/person belief) ; either pov or mind
                          subj (:belief/subject belief)
                          ]
                      [:li.belief
                       {:class (cond
                                 (:missing? belief)
                                 "text-muted"
                                 (and (:belief/lie? belief)
                                      (or (not playing?)
                                        (= pov (:belief/fabricator pov))))
                                 "belief-lie"
                                 :else
                                 (case (:belief/feeling belief)
                                   :like "bg-warning" ;; yellow
                                   :anger "bg-danger" ;; red
                                   :fear "bg-info" ;; blue
                                   ""))}
                       [(if (:missing? belief) :del :span)
                        (narr/phrase-belief db belief (:belief/mind belief) nil)]]))]
    [:div
     [:div.row
      [:div.col-lg-12
       (if pov
         [:p "Showing what " [:b (name pov)] " knows. "
          (when-not playing?
            [:button.btn-primary.btn-xs
             {:on-click
              (fn [_]
                (swap! ui-state assoc :current-pov nil))}
             "Show true feelings"])]
         (when-not playing?
           [:p.text-muted
            "Click a name to show what they know:"]))]]
     (into [:div.row]
           (for [mind people
                 :let [avatar (:person/avatar
                               (d/pull db '[*] [:person/id mind]))]]
             [:div.col-xs-6.col-sm-4.col-md-3.col-lg-2
              [:div.panel
               {:class (if (= mind pov) "panel-primary" "panel-default")}
               [:div.panel-heading
                [:h4.panel-title.pull-right
                 [:a
                  {:on-click
                   (fn [_]
                     (swap! ui-state update :choosing-avatar
                            #(if (= % mind) nil mind)))}
                  avatar]]
                [:h4.panel-title
                 (if (= mind pov)
                   (name mind)
                   [:a
                    {:href "#"
                     :on-click
                     (fn [_]
                       (swap! ui-state assoc :current-pov mind))}
                    (name mind)])
                 (if (and pov (not= mind pov))
                   [:small (str " according to " (name pov))])]
                ]
               [:div.panel-body
                (if (= mind (:choosing-avatar @ui-state))
                  (into
                   [:div
                    [:p.small "Pick an avatar:"]]
                   (for [a avatars]
                     [:a
                      {:on-click
                       (fn [_]
                         (swap! app-state update :db
                                d/db-with [{:person/id mind
                                            :person/avatar a}])
                         (swap! ui-state
                                assoc :choosing-avatar nil))}
                      a]))
                  (let [knowl (d/q gossip/my-knowledge-of-their-beliefs-q
                                   db (or pov mind) mind)
                        ;; also look up this mind's actual beliefs
                        ;; - see what knowledge is missing and tag it
                        missing (when (and pov (not= pov mind) (not playing?))
                                  (let [substance
                                        (fn [b]
                                          (select-keys b [:belief/mind
                                                          :belief/subject
                                                          :belief/object
                                                          :belief/feeling]))
                                        knowl* (set (map substance knowl))]
                                    (remove #(contains? knowl* (substance %))
                                            (d/q gossip/my-beliefs-q db mind))))
                        by-subj (->> knowl
                                     (concat (map #(assoc % :missing? true)
                                                  missing))
                                     (sort-by (juxt :belief/subject
                                                    :belief/object))
                                     (group-by :belief/subject))]
                    [:div
                     (if-let [bs (get by-subj mind)]
                       (into [:ul.list-unstyled]
                             (for [belief bs]
                               (belief-li mind belief)))
                       [:p.small
                        "I don't have any feelings."])
                     (if (seq (dissoc by-subj mind))
                       [:div
                        [:h5 "I think:"]
                        (into [:ul.list-unstyled]
                              (for [[subj bs] (dissoc by-subj mind)
                                    belief bs]
                                (belief-li mind belief)))]
                       [:p.small
                        "I don't know others' feelings."])
                     (let [likes (->> (d/q gossip/perceived-popularity-q
                                           db (or pov mind) mind)
                                      (sort-by second >))
                           maxlikes (second (first likes))
                           mostpop (->> likes
                                        (take-while #(= maxlikes (second %)))
                                        (map first))]
                       (if (>= maxlikes 2)
                         [:p
                          (str/join " and " (map name mostpop))
                          (if (> (count mostpop) 1) " are " " is ")
                          "most popular."]
                         [:p.small.text-muted
                          "I don't know who is most popular."]))]))]
               [:div.panel-footer
                ;; how popular am i really
                (when-not playing?
                  (when-let [likes (d/q gossip/my-true-popularity-q db mind)]
                    [:p.small {:style {:white-space "nowrap"}}
                     "Popularity " [:span.badge likes]]))
                ;; who am i indebted to
                (when-let [dto (cond->>
                                   (d/q gossip/indebted-to-q db mind)
                                 (and pov (not= mind pov)) ;; only what pov knows
                                 (filter #(= pov %))
                                 true
                                 (seq)
                                 )]
                  [:div.small
                   "I owe gossip to "
                   (str/join ", " (map name dto))]
                  )]]
              ]))]))

(defn turn-part-pane
  [part reply owing-after? playing?]
  (let [{:keys [speaker listener gossip existing reaction exposed-lie?]} part
        db (:db-before part)
        spe (name speaker)
        lis (name listener)
        owing-before? (gossip/indebted db speaker listener)]
    [:div
     ;; back-gossip if any - i.e. correcting outdated beliefs
     (when-let [backgoss (seq (:back-gossip part))]
       (into
        [:div]
        (for [[attempt corrected] backgoss]
          [:div
           [:p
            [:b (str spe ": ")]
            (narr/phrase-gossip db speaker listener attempt)]
           [:p
            [:b (str lis ": ")]
            (-> (rand-nth narr/correction-phrases)
                (replacem {"CORRECT" (narr/phrase-belief db corrected listener speaker)
                           }))
            ]]
          )))
     ;; the valid gossip if any (or the belief which was exposed as a lie)
     ;; first, prefix when owing:
     (when (and (not playing?)
                owing-before?
                (= speaker (:belief/fabricator gossip)))
       [:p
        [:i
         (str spe " knows " (he-she db speaker) " owes " lis
              " some goss. "
              "So " (he-she db speaker) " lies,")]])
     ;; valid gossip, if any
     (if gossip
       [:div
        [:p
         [:b (str spe ": ")]
         (narr/phrase-gossip db speaker listener gossip)
         ]
        ;; response
        [:p
         [:b (str lis ": ")]
         (narr/phrase-response db speaker listener gossip (assoc part :news? true))
         ]
        ]
       ;; no gossip
       [:div
        [:p
         [:b (str spe ": ")]
         (rand-nth narr/no-gossip-phrases)
         ]
        (when owing-after?
          [:p
           [:b (str lis ": ")]
           (rand-nth narr/no-gossip-response-phrases)])]
       )
     ;; minor news, if any - stuff the listener didn't know that speaker knew
     (when-let [minor (seq (:minor-news part))]
       [:div
        [:p
         [:b (str spe ": ")]
         (str "Oh and by the way. "
              (str/join " And "
                        (map (fn [b]
                               (str "I know that "
                                    (narr/phrase-belief db b speaker listener)))
                             minor)))]
        [:p
         [:b (str lis ": ")]
         (let [embarrased? (some #(and (= (:belief/subject %) listener)
                                       (= (:belief/object %) speaker))
                                 minor)]
           (if embarrased?
             (rand-nth narr/embarrassed-phrases)
             (rand-nth narr/minor-news-response-phrases)))]
        ])
     #_[:div.bg-success
      [:h5 "DATA"]
      [:ul
       [:li (str "gossip " (:gossip part))]
       [:li (str "existing " (:existing part))]
       [:li (str "exposed-lie? " (:exposed-lie? part))]
       [:li (str "reaction " (:reaction part))]
       [:li (str "back-gossip " (:back-gossip part))]
       [:li (str "minor-news " (:minor-news part))]
       ]]
     ]))

(defn avatar-float
  [db person float]
  (let [ava (:person/avatar
             (d/pull db '[*] [:person/id person]))]
    [:div
     {:style {:float float
              :padding "1em"
              :margin (case float
                        "left" "0 1em 1em 0"
                        "right" "0 0 1em 1em")
              :background "#eee"
              :border "1px solid #ddd"
              :border-radius "33%"}}
     [:div.text-center {:style {:font-size "5em"}}
      ava]
     [:h4.text-center (name person)]
     ]))

(defn interactive-attempt
  [state subject object feeling]
  (let [ienc (:interactive state)
        {:keys [db player partner meet-str attempts]} ienc
        belief* {:belief/person player
                 :belief/mind player
                 :belief/subject subject
                 :belief/object object
                 :belief/feeling feeling}
        existing (gossip/existing-belief db belief*)
        belief (if existing
                 existing
                 (assoc belief*
                        :belief/lie? true
                        :belief/fabricator player))
        result (gossip/goss-result db player partner belief)
        attempt [(assoc belief :phrased
                        (narr/phrase-gossip db player partner belief))
                 (assoc result :phrased
                        (narr/phrase-response db player partner belief result))]
        ;; if gossip was successful, clear player debt
        ;; if had back-gossip (because outdated/lie), clear partner debt
        db (cond-> (:db result)
             (:news? result) (gossip/update-debt player partner true)
             (:wrong? result) (gossip/update-debt partner player true))]
    (update state :interactive
            (fn [ienc]
              (-> ienc
                  (update :attempts conj attempt)
                  (assoc :db db))))))

(defn interactive-turn-pane
  [app-state ui-state]
  (let [db-before (:db @app-state)
        ienc (:interactive @app-state)
        {:keys [db player partner meet-str attempts]} ienc
        [gossip gossip-result] (some (fn [[b m]]
                                       (when (:news? m)
                                         [b m]))
                                     attempts)
        owing (gossip/indebted db player partner)]
    [:div
     (let [emo-str (narr/emotional-setting db-before player partner player)]
       (when-not (str/blank? emo-str)
         [:p
          [:i
           [:b (name player) " thinks: "]
           emo-str]]))
     [:p.lead
      meet-str]
     (avatar-float db player "left")
     (into [:div]
           (for [[belief result] attempts]
             [:div
              [:p
               [:b (str (name player) ": ")]
               (:phrased belief)]
              ;; response
              [:p
               [:b (str (name partner) ": ")]
               (:phrased result)
               ]
              ]))
     (if-not (:continued ienc)
       ;; this turn is still in play, interactive
       (let [belief (:play-belief @ui-state)
             {:keys [subject object feeling]} belief
             belief* {:belief/person player
                      :belief/mind player
                      :belief/subject subject
                      :belief/object object
                      :belief/feeling feeling}
             existing (when (and subject object)
                        (gossip/existing-belief db belief*))
             legit? (or (not object)
                        (= feeling (:belief/feeling existing :none)))]
         [:div
          [:p
           [:b (str (name player) ": ")]
           ]
          [:div.form-inline
           (belief-input ui-state :play-belief db false)
           ;; gossip button
           [:button.btn
            {:class (if legit? "btn-success" "btn-warning")
             :on-click
             (fn [_]
               (swap-advance! app-state interactive-attempt
                              subject object feeling)
               (swap! ui-state assoc-in [:play-belief :object] nil))
             :disabled (when (or (not (:subject belief))
                                 (not (:object belief))
                                 (= (:subject belief) (:object belief))
                                 (and (= (:subject belief) player)
                                      (= (:object belief) partner)))
                         "disabled")}
            (if legit? "Try gossip" "Lie!")]]
          ;; check whether in debt to listener
          (if owing
            [:p
             [:i
              (str "You owe " (name partner) " some goss. You can't skip!")]]
            [:div.form-inline
             ;; continue with rest of turn button
             [:button.btn.btn-default
              {:on-click
               (fn [_]
                 ;; fall into debt if didn't gossip
                 (let [db (gossip/update-debt db player partner gossip)
                       fwd-part (assoc gossip-result
                                       :db db
                                       :gossip gossip
                                       :speaker player
                                       :listener partner)
                       cont (-> (gossip/continue-turn db player partner fwd-part)
                                (gossip/partner-think))]
                   (swap-advance! app-state assoc-in [:interactive :continued]
                                  cont)))
               }
              (if gossip
                "How about you?"
                "I got nothing, sorry")]
             ])
          (avatar-float db partner "right")
          ])
       ;; continued turn
       (let [enc (:continued ienc)
             ini player
             par partner
             ]
         [:div
          (when (:gossip (:fwd-cause-part enc))
            [:div
             [:p
              [:b (str (name par) ": ")]
              "Wait, what?"]
             (turn-part-pane (:fwd-cause-part enc)
                             "" false true)])
          (avatar-float db partner "right")
          (turn-part-pane (:back-part enc)
                          (:back-reply enc)
                          (gossip/indebted db par ini)
                          true)
          (when (:gossip (:back-cause-part enc))
            [:div
             [:p
              [:b (str (name ini) ": ")]
              "Wait, what?"]
             (turn-part-pane (:back-cause-part enc)
                             "" false true)])
          ])
       )]))

(defn other-turn-playing-pane
  [enc db-before playing-as]
  (let [db (:db enc)
        ini (:initiator enc)
        par (:partner enc)
        ini-ava (:person/avatar
                 (d/pull db '[*] [:person/id ini]))
        par-ava (:person/avatar
                 (d/pull db '[*] [:person/id par]))]
    [:div
     [:div ;.col-md-4
      (let [emo-str (narr/emotional-setting db-before ini par playing-as)]
        (when-not (str/blank? emo-str)
          [:p
           [:i
            [:b (name playing-as) " thinks: "]
            emo-str]]))
      [:p.lead
       (:meet-phrase enc)]
      (avatar-float db ini "left")
      [:p.text-muted
       (apply str "Rhubarb"
              (repeat 50 " rhubarb"))]
      (avatar-float db par "right")
      [:p.text-muted
       (apply str "Rhubarb"
              (repeat 50 " rhubarb"))]
      ]]))

(defn other-turn-not-playing-pane
  [enc db-before]
  (let [db (:db enc)
        ini (:initiator enc)
        par (:partner enc)
        ini-ava (:person/avatar
                 (d/pull db '[*] [:person/id ini]))
        par-ava (:person/avatar
                 (d/pull db '[*] [:person/id par]))]
    [:div
     [:div ;.col-md-4
      [:p
       [:i
        (narr/emotional-setting db-before ini par nil)
        ]]
      [:p.lead
       (:meet-phrase enc)]
      (avatar-float db ini "left")
      (turn-part-pane (:fwd-part enc)
                      (:fwd-reply enc)
                      (gossip/indebted db ini par)
                      false)
      (when (:gossip (:fwd-cause-part enc))
        [:div
         [:p
          [:b (str (name par) ": ")]
          "Wait, what?"]
         (turn-part-pane (:fwd-cause-part enc)
                         "" false false)])
      ;; listener's thoughts after the interaction
      (when-let [thoughts (seq (:fwd-thoughts enc))]
        [:p
         [:i
          [:b (str (name par) " thinks: ")]
          (->> thoughts
               (map :belief/phrase)
               (str/join \newline))]])
      (avatar-float db par "right")
      (turn-part-pane (:back-part enc)
                      (:back-reply enc)
                      (gossip/indebted db par ini)
                      false)
      (when (:gossip (:back-cause-part enc))
        [:div
         [:p
          [:b (str (name par) ": ")]
          "Wait, what?"]
         (turn-part-pane (:back-cause-part enc)
                         "" false false)])
      ;; listener's thoughts after the interaction
      (when-let [thoughts (seq (:back-thoughts enc))]
        [:p
         [:i
          [:b (str (name par) " thinks: ")]
          (->> thoughts
               (map :belief/phrase)
               (str/join \newline))]])
      ;; later thoughts
      (when-let [thoughts (seq (:partner-thoughts enc))]
        [:p
         [:i
          [:b (str (name par) " thinks: ")]
          (->> thoughts
               (map :belief/phrase)
               (str/join \newline))]
         ])
      (when-let [thoughts (seq (:initiator-thoughts enc))]
        [:p
         [:i
          [:b (str (name ini) " thinks: ")]
          (->> thoughts
               (map :belief/phrase)
               (str/join \newline))]])
      ]
     ]))

(defn encounter-pane
  [app-state ui-state]
  (let [db (:db @app-state)
        people (gossip/all-people db)]
    [:div
     (when (and (>= (count people) 3)
                (not (:encounter @app-state))
                (not (:interactive @app-state)))
       [:div.row
        [:div.col-lg-12
         [:button.btn.btn-primary.btn-block
          {:on-click (fn [_]
                       (if-let [player (:playing-as @ui-state)]
                         (let [one (rand-nth people)
                               other (gossip/random-partner db one)
                               [ini par] (cond
                                           (= one player) [one other]
                                           (= other player) [other one]
                                           :else [one other])]
                           (if (= ini player)
                             ;; interactive turn
                             (swap-advance! app-state assoc
                                            :interactive (init-interactive
                                                          db ini par)
                                            :day (inc (:day @app-state 0)))
                             ;; not our turn
                             (let [turn (gossip/turn db ini par)]
                               (swap-advance! app-state assoc
                                             :encounter (narr/narrate-turn turn)
                                             :day (inc (:day @app-state 0)))))
                           )
                         ;; not playing
                         (let [turn (gossip/random-turn db)]
                           (swap-advance! app-state assoc
                                          :encounter (narr/narrate-turn turn)
                                          :day (inc (:day @app-state 0))))))
           }
          "Next encounter"
          ]
         [:p.lead
          "You can add feelings or people now."]]])
     (when (or (:encounter @app-state)
               (:continued (:interactive @app-state)))
       [:div.row
        [:div.col-lg-12
         [:button.btn.btn-success.btn-block
          {:on-click (fn [_]
                       (if-let [enc (or (:encounter @app-state)
                                        (:continued (:interactive @app-state)))]
                         (swap-advance! app-state assoc
                                        :db (:db enc)
                                        :encounter nil
                                        :interactive nil)))
           }
          "Continue"
          ]]])
     (when (:interactive @app-state)
       (interactive-turn-pane app-state ui-state))
     (when-let [enc (:encounter @app-state)]
       (if-let [playing-as (:playing-as @ui-state)]
         (other-turn-playing-pane enc db playing-as)
         (other-turn-not-playing-pane enc db)))
     ]))

(defn navbar
  [app-state ui-state]
  [:nav.navbar.navbar-default
   [:div.container-fluid
    [:div.navbar-header
     [:a.navbar-brand {:href "https://github.com/floybix/gossip"}
      "Gossip."]]
    [:div
     [:ul.nav.navbar-nav
      ;; step back
      [:li
       [:button.btn.btn-default.navbar-btn
        {:type :button
         :on-click
         (fn [_]
           (let [new-state (peek @undo-buffer)]
             (swap! undo-buffer pop)
             (swap! redo-buffer conj @app-state)
             (reset! app-state new-state)))
         :title "Step backward in time"
         :disabled (when (empty? @undo-buffer) "disabled")}
        [:span.glyphicon.glyphicon-step-backward {:aria-hidden "true"}]
        [:span.visible-xs-inline " Step backward"]]]
      ;; step forward
      (when-not (empty? @redo-buffer)
        [:li
         [:button.btn.btn-default.navbar-btn
          {:type :button
           :on-click
           (fn [_]
             (let [new-state (peek @redo-buffer)]
               (swap! redo-buffer pop)
               (swap! undo-buffer conj @app-state)
               (reset! app-state new-state)))
           :title "Step forward in time"
           :disabled (when (empty? @redo-buffer) "disabled")}
          [:span.glyphicon.glyphicon-step-forward {:aria-hidden "true"}]
          [:span.visible-xs-inline " Step forward"]]])
      ;; timestep
      [:li
       [:p.navbar-text
        (str " Day " (:day @app-state))]]
      ;; playing as
      (let [db (:db @app-state)
            people (gossip/all-people db)]
        [:form.navbar-form.navbar-left
         [:div.form-group
          [:label
           "Playing as: "]
          [:select.form-control
           {:value (name (or (:playing-as @ui-state) "not playing"))
            :on-change (fn [e]
                         (let [s (-> e .-target forms/getValue)
                               new-as (when-not (= s "not playing") (keyword s))]
                           (swap! ui-state assoc
                                  :playing-as new-as)
                           (when new-as
                             (swap! ui-state assoc-in
                                    [:adding-belief :subject] new-as))))}
           (for [person (cons "not playing" people)]
             [:option {:key (name person)
                       :value (name person)}
              (name person)])
           ]
          ]])
      (when (:playing-as @ui-state)
        [:li
         [:button.btn.btn-default.navbar-btn
          {:type :button
           :on-click
           (fn [_]
             (swap! ui-state assoc
                    :playing-as nil))
           }
          "End game"]])
      ]
     ;; right-aligned items
     [:ul.nav.navbar-nav.navbar-right
      ;; dedication
      [:li
       [:p.navbar-text "made with "
        [:small {:style {:margin-right "0.5ex"}} "💛"]
        " for Zari Andrews."]]]
     ]]])

(defn app-pane
  [app-state ui-state]
  [:div
   [navbar app-state ui-state]
   [:div.container-fluid
    ;; can not alter db when it will be replaced:
    (when-not (or (:encounter @app-state)
                  (:interactive @app-state))
      [:div.row
       [:div.col-lg-6
        [add-person-pane app-state ui-state]]
       [:div.col-lg-6
        [add-belief-pane app-state ui-state]]])
    [:div.row
     [:div.col-lg-8.col-md-6
      [status-pane app-state ui-state]]
     [:div.col-lg-4.col-md-6
      [encounter-pane app-state ui-state]]]
    ]
   ])

(reagent/render-component [app-pane app-state ui-state]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
