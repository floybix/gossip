(ns gossip.app
  (:require [reagent.core :as reagent :refer [atom]]
            [goog.dom.forms :as forms]
            [datascript.core :as d]
            [gossip.core :as gossip :refer [he-she him-her replacem]]
            [gossip.narrative :as narr]
            [gossip.viz :as viz]
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
         :started? false
         :playing-as nil
         :avatars {}
         :choosing-avatar nil
         :graph-coords {}
         :adding-person {:name ""
                         :male? false}
         :adding-belief {:belief/subject nil
                         :belief/object nil
                         :belief/feeling :like}
         :play-belief {:belief/subject nil
                       :belief/object nil
                       :belief/feeling :like}}))

(defn init-interactive
  [db player partner]
  {:player player
   :partner partner
   :db db
   :meet-str (narr/meeting-phrase db player partner)
   :attempts []})

(defn init-beliefs
  [db]
  (let [people (gossip/all-people db)
        news (mapcat (fn [person]
                       (if (seq (d/q gossip/my-feelings-q db person))
                         ;; has feelings already.
                         []
                         ;; no feelings. create one.
                         (let [others (remove #{person} people)
                               object (rand-nth others)
                               feeling (rand-nth [:like :like :fear :anger])]
                           [(gossip/->belief person person person object feeling)])))
                     people)]
    (if (seq news)
      (d/db-with db news)
      db)))

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
               avatar (rand-nth avatars)
               people (gossip/all-people (:db @app-state))
               first? (empty? people)]
           (swap-advance! app-state update :db
                          d/db-with [{:person/id id
                                      :person/gender gender}])
           (swap! ui-state
                  (fn [m]
                    (-> (if first?
                          (assoc m :playing-as id)
                          m)
                        (assoc-in [:avatars id] avatar)
                        (assoc-in [:graph-coords id] [(+ 50 (rand-int 200))
                                                      (+ 50 (rand-int 100))])
                        (assoc-in [:adding-person :name] ""))))))
       :disabled (when (str/blank? (:name person))
                   "disabled")}
      "Add person"]]))

(defn belief-input
  [ui-state ui-state-key db]
  (let [belief (get @ui-state ui-state-key)
        people (gossip/all-people db)]
    [:div.form-group
      ;; subject
      [:select.form-control
       {:value (or (:belief/subject belief) " ")
        :on-change (fn [e]
                     (let [s (-> e .-target forms/getValue)]
                       (swap! ui-state assoc-in
                              [ui-state-key :belief/subject]
                              (when-not (str/blank? s) (keyword s)))))}
       (for [person (cons " " people)]
         [:option {:key person
                   :value person}
          (name person)])
       ]
      ;; feeling
      [:select.form-control
       {:value (:belief/feeling belief)
        :on-change (fn [e]
                     (let [s (-> e .-target forms/getValue)]
                       (swap! ui-state assoc-in
                              [ui-state-key :belief/feeling]
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
       {:value (or (:belief/object belief) " ")
        :on-change (fn [e]
                     (let [s (-> e .-target forms/getValue)]
                       (swap! ui-state assoc-in
                              [ui-state-key :belief/object]
                              (when-not (str/blank? s) (keyword s)))))}
       (for [person (cons " " people)]
         [:option {:key person
                   :value person}
          (name person)])
       ]]))

(defn add-belief-pane
  [app-state ui-state]
  (let [belief (:adding-belief @ui-state)
        subject (:belief/subject belief)
        object (:belief/object belief)
        db (:db @app-state)]
    [:div.form-inline.well
     (belief-input ui-state :adding-belief db)
     ;; add button
     [:button.btn.btn-default
      {:on-click
       (fn [_]
         (let [belief (assoc belief
                             :belief/person subject
                             :belief/mind subject)]
           (swap-advance! app-state update :db gossip/believe belief)
           (swap! ui-state assoc-in [:adding-belief :belief/object] nil)))
       :disabled (when (or (not subject)
                           (not object)
                           (= subject object))
                   "disabled")}
      "Add/replace feeling"]]))

(defn belief-li
  [db pov mind belief on-click]
  (let [mind (or mind (:belief/mind belief))]
    [:li.belief
     {:class (cond
               (:missing? belief)
               "text-muted"
               (and (:belief/lie? belief)
                    (or (not pov)
                        (= pov (:belief/fabricator belief))))
               "belief-lie"
               :else
               (case (:belief/feeling belief)
                 :like "bg-warning" ;; yellow
                 :anger "bg-danger" ;; red
                 :fear "bg-info" ;; blue
                 ""))
      :title (narr/belief-explanation db pov belief)}
     [(cond
        (:missing? belief) :del
        on-click :u
        :else :span)
      (when on-click
        {:on-click on-click
         :style {:cursor "pointer"}})
      (narr/phrase-belief db belief mind nil)]]))

(defn status-pane
  [app-state ui-state]
  (let [db (:db @app-state)
        playing? (:playing-as @ui-state)
        pov (or playing? (:current-pov @ui-state))
        people (gossip/all-people db)]
    [:div
     [:div.row
      [:div.col-lg-12
       [:p
        "Drag people. Hover on beliefs for info. "
        (if pov
          [:span
           "Showing what " [:b (name pov)] " knows. "
           (when-not playing?
             [:button.btn-primary.btn-xs
              {:on-click
               (fn [_]
                 (swap! ui-state assoc :current-pov nil))}
              "Show true feelings"])]
          (when-not playing?
            "Click a name to show what they know:"))]]]
     [:div.row
      [:div.col-lg-12
       (viz/social-graph-svg ui-state db pov)]
      ]
     [:p "Click avatars to change them. Hover on beliefs for info."]
     (into [:div.row]
           (for [mind people
                 :let [avatar (get-in @ui-state [:avatars mind] "@")]]
             [:div.col-xs-6.col-sm-4.col-md-3.col-lg-2
              [:div.panel
               {:class (if (= mind pov) "panel-primary" "panel-default")}
               [:div.panel-heading
                [:h4.panel-title.pull-right
                 [:a
                  {:on-click
                   (fn [_]
                     (swap! ui-state update :choosing-avatar
                            #(if (= % mind) nil mind)))
                   :href "#"}
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
                  [:div
                   [:p.small "Pick an avatar:"]
                   (into
                    [:div
                     {:style {:font-size "18px"}}]
                    (for [a avatars]
                      [:a
                       {:on-click
                        (fn [_]
                          (swap! ui-state assoc-in [:avatars mind] a)
                          (swap! ui-state assoc :choosing-avatar nil))}
                       a]))]
                  (let [knowl (d/q gossip/my-knowledge-of-their-beliefs-q
                                   db (or pov mind) mind)
                        ;; also look up this mind's actual beliefs
                        ;; - see what knowledge is missing and tag it
                        missing (when (and pov (not= pov mind) (not playing?))
                                  (let [knowl* (set (map gossip/substance knowl))]
                                    (remove #(contains? knowl* (gossip/substance %))
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
                               (belief-li db pov mind belief nil)))
                       [:p.small
                        "I don't have any feelings."])
                     (if (seq (dissoc by-subj mind))
                       [:div
                        [:h5 "I think:"]
                        (into [:ul.list-unstyled]
                              (for [[subj bs] (dissoc by-subj mind)
                                    belief bs]
                                (belief-li db pov mind belief nil)))]
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
              ]))
     ]))

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
     ]))

(defn avatar-float
  [ui-state person float]
  (let [ava (get-in @ui-state [:avatars person])]
    ;; need css transform/absolute hack to scale emoji on ios
    [:div
     {:style {:float float
              :width "5em"
              :height "5em"
              :padding "1em"
              :margin (case float
                        "left" "0 0 0 1em"
                        "right" "1em 0 0 1em"
                        )
              }}
     [:div
      {:style {:-webkit-transform "scale(5)"
               :-moz-transform "scale(5)"
               :transform "scale(5)"
               :position "absolute"}}
      ava]
     " "
     ]))

(defn interactive-attempt
  [state belief*]
  (let [ienc (:interactive state)
        {:keys [db player partner meet-str attempts]} ienc
        existing (gossip/existing-belief db belief*)
        belief (if (and existing
                        (= (:belief/feeling belief* :none)
                           (:belief/feeling existing :none)))
                 existing
                 (assoc belief*
                        :belief/lie? true
                        :belief/fabricator player))
        result (gossip/goss-result db player partner belief)
        ;; if gossip was successful, clear player debt
        ;; if had back-gossip (because outdated/lie), clear partner debt
        db (cond-> (:db result)
             (:news? result) (gossip/update-debt player partner true)
             (:wrong? result) (gossip/update-debt partner player true))
        [db thoughts] (gossip/think db player)
        attempt [(assoc belief :phrased
                        (narr/phrase-gossip db player partner belief))
                 (assoc result :phrased
                        (narr/phrase-response db player partner belief result))
                 thoughts]]
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
     (avatar-float ui-state player "left")
     (into [:div]
           (for [[belief result thoughts] attempts]
             [:div
              [:p
               [:b (str (name player) ": ")]
               (:phrased belief)]
              ;; response
              [:p
               [:b (str (name partner) ": ")]
               (:phrased result)
               ]
              (when (seq thoughts)
                [:p
                 [:i
                  [:b (str (name player) " thinks: ")]
                  (->> thoughts
                       (map :belief/phrase)
                       (str/join \newline))]])
              ]))
     (if-not (:continued ienc)
       ;; this turn is still in play, interactive
       (let [belief (assoc (:play-belief @ui-state)
                           :belief/person player
                           :belief/mind player)
             subject (:belief/subject belief)
             object (:belief/object belief)
             existing (when (and subject object)
                        (gossip/existing-belief db belief))
             checking? (= subject partner)
             legit? (or (not object) ;; so default display is green
                        (= (:belief/feeling belief :none)
                           (:belief/feeling existing :none)))]
         [:div
          [:p
           [:b (str (name player) ": ")]
           ]
          [:div.form-inline
           (belief-input ui-state :play-belief db)
           ;; gossip button
           [:button.btn
            {:class (if legit? "btn-success" "btn-warning")
             :on-click
             (fn [_]
               (swap-advance! app-state interactive-attempt belief)
               (swap! ui-state assoc-in [:play-belief :belief/object] nil))
             :disabled (when (or (not subject)
                                 (not object)
                                 (= subject object)
                                 ;; no direct feelings
                                 (and (= subject player)
                                      (= object partner))
                                 ;; can not lie about listener directly!
                                 (and (= subject partner)
                                      (not legit?))
                                 ;; can not lie about own feelings
                                 (and (= subject player)
                                      (not legit?)))
                         "disabled")}
            (if legit?
              (if checking? "Check gossip" "Try gossip")
              "Lie!")]
           (when (and (= subject player)
                      (= object partner))
             [:p.text-danger "No direct feelings!"])
           (when (and (= subject partner)
                      (not legit?))
             [:p.text-danger "Can not lie about the listener!"])
           (when (and (= subject player)
                      (not legit?))
             [:p.text-danger "Can not lie about your own feelings!"])
           ]
          ;; check whether in debt to listener
          (if owing
            [:p
             [:i
              (str "You owe " (name partner) " some goss. You can't skip!")]]
            [:div.form-inline
             ;; continue with rest of turn button
             [:button.btn
              {:class (if gossip "btn-primary" "btn-default")
               :on-click
               (fn [_]
                 ;; fall into debt if didn't gossip
                 (let [db (gossip/update-debt db player partner gossip)
                       fwd-part (assoc gossip-result
                                       :db db
                                       :gossip gossip
                                       :speaker player
                                       :listener partner)
                       cont (-> (gossip/continue-turn db player partner fwd-part)
                                (gossip/partner-think)
                                (gossip/initiator-think))]
                   (swap-advance! app-state assoc-in [:interactive :continued]
                                  cont)))
               }
              (if gossip
                "How about you?"
                "I got nothing, sorry")]
             ])
          (avatar-float ui-state partner "right")
          ;; shortcuts, listing known beliefs
          (let [their-knowl (d/q gossip/my-knowledge-of-their-beliefs-q
                                 db player partner)
                my-knowl (d/q gossip/my-beliefs-q
                              db player)
                ;; find any of my beliefs they don't already know
                new-knowl (let [their* (set (map gossip/substance their-knowl))]
                            (remove #(contains? their* (gossip/substance %))
                                    my-knowl))]
            [:div
             {:style {:clear "left"}}
             [:h5 "Shortcuts"]
             (if-let [bs (seq new-knowl)]
               [:div
                {:style {:clear "left"}}
                [:p
                 "I think " (name partner) " does not know these:"]
                (into [:ul.list-unstyled
                       {:style {:width "24ex"
                                :float "left"}}]
                      (for [belief bs]
                        (belief-li db player player belief
                                   (fn []
                                     (swap! ui-state assoc :play-belief
                                            (gossip/substance belief))))))
                ]
               )
             (if-let [bs (seq their-knowl)]
               [:div
                {:style {:clear "left"}}
                [:p
                 "As far as I know, " (name partner) " already thinks these, but can check:"]
                (into [:ul.list-unstyled
                       {:style {:width "24ex"
                                :float "left"}}]
                      (for [belief bs]
                        (belief-li db player player belief
                                   (fn []
                                     (swap! ui-state assoc :play-belief
                                            (gossip/substance belief))))))])
             [:p
              {:style {:clear "left"}}
              "Or, of course, I could try something else."]])
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
          (avatar-float ui-state partner "right")
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
          ;; listener's thoughts after the interaction
          (when-let [thoughts (seq (:back-thoughts enc))]
            [:p
             [:i
              [:b (str (name player) " thinks: ")]
              (->> thoughts
                   (map :belief/phrase)
                   (str/join \newline))]])
          ;; later thoughts
          (when-let [thoughts (seq (:initiator-thoughts enc))]
            [:p
             [:i
              [:b (str (name ini) " thinks: ")]
              (->> thoughts
                   (map :belief/phrase)
                   (str/join \newline))]])
          ])
       )]))

(defn other-turn-playing-pane
  [ui-state enc db-before playing-as]
  (let [db (:db enc)
        ini (:initiator enc)
        par (:partner enc)]
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
      (avatar-float ui-state ini "left")
      [:p.text-muted
       (apply str "Rhubarb"
              (repeat 50 " rhubarb"))]
      (avatar-float ui-state par "right")
      [:p.text-muted
       (apply str "Rhubarb"
              (repeat 50 " rhubarb"))]
      ]]))

(defn other-turn-not-playing-pane
  [ui-state enc db-before]
  (let [db (:db enc)
        ini (:initiator enc)
        par (:partner enc)]
    [:div
     [:div ;.col-md-4
      [:p
       [:i
        (narr/emotional-setting db-before ini par nil)
        ]]
      [:p.lead
       (:meet-phrase enc)]
      (avatar-float ui-state ini "left")
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
      (avatar-float ui-state par "right")
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
          [:b (str (name ini) " thinks: ")]
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
     (when (and (not (:encounter @app-state))
                (not (:interactive @app-state)))
       [:div
        (when (and (>= (count people) 3)
                   (or (not (:playing-as @ui-state))
                       (:started? @ui-state)))
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
           ])
        [:p.lead
         (cond
           (< (count people) 3)
           "Add people. You need at least 3 characters."
           (and (:playing-as @ui-state)
                (not (:started? @ui-state)))
           "Ready when you are... add more people if you want."
           (:playing-as @ui-state)
           "Keep going with the game. End it when you've had enough."
           :else ;; not playing (e.g. finished)
           "You are not playing. You can still simulate encounters though."
           )]])
     (when (or (:encounter @app-state)
               (:continued (:interactive @app-state)))
       [:div
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
         ]])
     (when (:interactive @app-state)
       [interactive-turn-pane app-state ui-state])
     (when-let [enc (:encounter @app-state)]
       (if-let [playing-as (:playing-as @ui-state)]
         [other-turn-playing-pane ui-state enc db playing-as]
         [other-turn-not-playing-pane ui-state enc db]))
     ]))

(defn navbar
  [app-state ui-state]
  (let [db (:db @app-state)
        people (gossip/all-people db)]
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
          " Undo"]]
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
            " Redo"]])
        ;; timestep
        [:li
         [:p.navbar-text
          (str " Day " (:day @app-state))]]
        ;; playing as
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
                                    [:adding-belief :belief/subject] new-as)
                             (swap! ui-state assoc :final-results nil)
                             )))
            }
           (doall
            (for [person (cons "not playing" people)]
              [:option {:key (name person)
                        :value (name person)}
               (name person)]))
           ]
          ]]
        (when (and (:playing-as @ui-state)
                   (not (:started? @ui-state))
                   (>= (count people) 3))
          [:li
           [:button.btn.btn-primary.navbar-btn
            {:type :button
             :on-click
             (fn [_]
               (swap! ui-state assoc :started? true)
               (swap-advance! app-state update :db init-beliefs)
               )
             }
            "Start game playing as " [:b (name (:playing-as @ui-state))]]])
        (when (and (:playing-as @ui-state)
                   (:started? @ui-state))
          [:li
           [:button.btn.btn-default.navbar-btn
            {:type :button
             :on-click
             (fn [_]
               (let [db (:db @app-state)
                     people (gossip/all-people db)
                     popus (->>
                            (for [x people]
                              [x (or (d/q gossip/my-true-popularity-q db x)
                                     0)])
                            (sort-by val >))]
                 (swap! ui-state assoc :final-results popus)
                 (swap! ui-state assoc :playing-as nil, :started? false))
               )
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
       ]]]))

(defn final-results-pane
  [ui-state popus]
  [:div
   [:h3
    "Final popularity scores "
    [:small
     [:a
      {:href "#"
       :on-click
       (fn [_]
         (swap! ui-state assoc :final-results nil))}
      "[x]"]]]
   (into [:ul]
         (for [[index [person likes]] (map-indexed vector popus)]
           [:li.lead
            (name person)
            " was liked by "
            [:span.badge likes]
            " ... "
            (cond
              (zero? index) "🎉"
              (== index 1) "🎻"
              (or (== index (dec (count popus))) (zero? likes)) "💩")]))])

(defn app-pane
  [app-state ui-state]
  [:div
   [navbar app-state ui-state]
   [:div.container-fluid
    (when-let [popus (:final-results @ui-state)]
      [final-results-pane ui-state popus])
    ;; can not alter db when it will be replaced:
    (when-not (or (:encounter @app-state)
                  (:interactive @app-state)
                  (:started? @ui-state))
      [:div.row
       [:div.col-lg-6
        [add-person-pane app-state ui-state]]
       ;; only show belief controls when explicitly not playing
       (when (and (seq (:avatars @ui-state))
                  (not (:playing-as @ui-state)))
         [:div.col-lg-6
          [add-belief-pane app-state ui-state]])]
      )
    [:div.row
     [:div.col-lg-4.col-md-6
      [encounter-pane app-state ui-state]]
     [:div.col-lg-8.col-md-6
      [status-pane app-state ui-state]]
     ]
    ]
   ])

(reagent/render-component [app-pane app-state ui-state]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
