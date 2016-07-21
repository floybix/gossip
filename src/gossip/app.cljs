(ns gossip.app
  (:require [reagent.core :as reagent :refer [atom]]
            [goog.dom.forms :as forms]
            [datascript.core :as d]
            [gossip.core :as gossip]
            [gossip.narrative :as narr]
            [clojure.set :as set]
            [clojure.string :as str]))

(enable-console-print!)

(defonce app-state
  (atom {:db (d/empty-db gossip/db-schema)
         :encounter nil
         :adding-person {:name ""
                         :male? false}
         :adding-belief {:subject nil
                         :object nil
                         :feeling :like}}))

(def avatars
  (let [humans ["👦" "👧" "👨" "👩" "👱" "👲" "👳" "👴" "👵" "👶" "👸" "🙍" "🙎" "💁" "🙋" "💂" "🕵" "🎅" "👼" "👷" "👮" "👰" "🙇"]
        skins ["🏾" "🏻"]
        others ["👹" "👺" "👻" "👽" "👿" "😺" "😼" "🐤" "🐥" "🐦" "🐧" "🐨" "🐬" "🐭" "🐮" "🐯" "🐰" "🐱" "🐲" "🐳" "🐴" "🐵" "🐶" "🐷" "🐸" "🐹" "🐺" "🐻" "🐼" "🐽" "💩"
]]
    (vec (concat humans
                 (for [skin skins
                       x humans]
                   (str x skin))
                 others))))

(defn add-person-pane
  [app-state]
  (let [person (:adding-person @app-state)]
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
                       (swap! app-state assoc-in
                              [:adding-person :name]
                              s)))}]]
     ;; gender
     [:div.checkbox
      [:label
       [:input
        {:type :checkbox
         :checked (if (:male? person) true)
         :on-change (fn [_]
                      (swap! app-state update-in
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
               avatar (if (:male? person) (first avatars) (second avatars))]
           (swap! app-state update :db
                  d/db-with [{:person/id id
                              :person/gender gender
                              :person/avatar avatar}])
           (swap! app-state assoc-in [:adding-person :name] "")))
       :disabled (when (str/blank? (:name person)) "disabled")}
      "Add person"]]))

(defn add-belief-pane
  [app-state]
  (let [belief (:adding-belief @app-state)
        db (:db @app-state)
        people (gossip/all-people db)]
    [:div.form-inline.well
     [:div.form-group
      ;; subject
      [:select.form-control
       {:on-change (fn [e]
                     (let [s (-> e .-target forms/getValue)]
                       (swap! app-state assoc-in
                              [:adding-belief :subject]
                              (when (seq s) (keyword s)))))}
       (for [person (cons "" people)]
         [:option {:key person
                   :value person
                   :selected (if (= person (:subject belief)) "selected")
                   }
          (name person)])
       ]
      ;; feeling
      [:select.form-control
       {:on-change (fn [e]
                     (let [s (-> e .-target forms/getValue)]
                       (swap! app-state assoc-in
                              [:adding-belief :feeling]
                              (keyword s))))}
       (for [feeling [:like :anger :fear]]
         [:option {:key feeling
                   :value feeling
                   :selected (if (= feeling (:feeling belief)) "selected")
                   }
          (case feeling
            :like "likes"
            :anger "angry with"
            :fear "fears")])
       ]
      ;; object
      [:select.form-control
       {:on-change (fn [e]
                     (let [s (-> e .-target forms/getValue)]
                       (swap! app-state assoc-in
                              [:adding-belief :object]
                              (when (seq s) (keyword s)))))}
       (for [person (cons "" people)]
         [:option {:key person
                   :value person
                   :selected (if (= person (:object belief)) "selected")
                   }
          (name person)])
       ]]
     ;; add button
     [:button.btn.btn-default
      {:on-click
       (fn [_]
         (let [{:keys [subject object feeling]} belief]
           (swap! app-state update :db
                  gossip/believe subject subject subject object feeling)
           (swap! app-state assoc-in [:adding-belief :object] nil)))
       :disabled (when-not (and (:subject belief)
                                (:object belief))
                   "disabled")}
      "Add feeling"]]))

(defn status-pane
  [app-state]
  (let [db (:db @app-state)
        pov (:current-pov @app-state)
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
                                 (:belief/lie? belief)
                                 "belief-lie"
                                 :else
                                 (case (:belief/feeling belief)
                                   :like "bg-warning" ;; yellow
                                   :anger "bg-danger" ;; red
                                   :fear "bg-info" ;; blue
                                   ))}
                       [(if (:missing? belief) :del :span)
                        (narr/phrase-belief db belief (:belief/mind belief) nil)]]))]
    [:div
     [:div.row
      [:div.col-lg-12
       (if pov
         [:p "Showing what " [:b (name pov)] " knows. "
          [:button.btn-primary.btn-xs
           {:on-click
            (fn [_]
              (swap! app-state assoc :current-pov nil))}
           "Show true feelings"]]
         [:p.text-muted
          "Click a name to show what they know:"])]]
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
                     (swap! app-state assoc :choosing-avatar mind))}
                  avatar]]
                [:h4.panel-title
                 (if (= mind pov)
                   (name mind)
                   [:a
                    {:href "#"
                     :on-click
                     (fn [_]
                       (swap! app-state assoc :current-pov mind))}
                    (name mind)])
                 (if (and pov (not= mind pov))
                   [:small (str " according to " (name pov))])]
                ]
               [:div.panel-body
                (if (= mind (:choosing-avatar @app-state))
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
                         (swap! app-state
                                assoc :choosing-avatar nil))}
                      a]))
                  (let [knowl (d/q gossip/my-knowledge-of-their-beliefs-q
                                  db (or pov mind) mind)
                       ;; also look up this mind's actual beliefs
                       ;; - see what knowledge is missing and tag it
                       missing (when (and pov (not= pov mind))
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
                    (let [pops (->> (d/q gossip/perceived-popularity-q
                                         db (or pov mind) mind)
                                    (sort-by second >))
                          maxpop (second (first pops))
                          mostpop (->> pops
                                       (take-while #(= maxpop (second %)))
                                       (map first))]
                      (if (>= maxpop 2)
                        [:p
                         (str/join " and " (map name mostpop))
                         (if (> (count mostpop) 1) " are " " is ")
                         "most popular."]
                        [:p.small
                         "I don't know who is most popular."]))]))]
               [:div.panel-footer
                (let [dto (d/q gossip/indebted-to-q db mind)]
                  ;; how popular am i really
                  ;; who am i indebted to
                  (when (seq dto)
                    [:small "I owe gossip to "
                     (str/join ", " (map name dto))])
                  )]]
              ]))]))

(defn turn-part-pane
  [part thoughts reply]
  (let [{:keys [speaker listener gossip minor-news]} part
        db (:db-before part)
        spe (name speaker)
        lis (name listener)]
    [:div
     (when (gossip/indebted db speaker listener)
       [:p
        [:i
         (str spe " knows " (gossip/he-she db speaker) " owes " lis
              " some goss. "
              (if (:belief/lie? gossip)
                (str "In desperation, " (gossip/he-she db speaker) " makes something up:")
                (str "Luckily, " (gossip/he-she db speaker) " has some:")))]])
     [:p
      [:b (str spe ": ")]
      (str \"
           (if gossip
             (narr/phrase-gossip db speaker listener gossip)
             "I got nothing, sorry.")
           \")]
     (when (seq thoughts)
       [:p
        [:i
         [:b (str lis " thinks: ")]
         (->> thoughts
              (map :belief/phrase)
              (str/join \newline))]])
     [:p
      [:b (str lis ": ")]
      (str \"
           (if gossip
             reply
             "You owe me, ok?")
           \")]
     [:div.bg-success
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

(defn encounter-pane
  [app-status]
  (let [db (:db @app-state)
        people (gossip/all-people db)]
    [:div
     (when (and (>= (count people) 2)
                (not (:encounter @app-state)))
       [:div.row
        [:div.col-lg-12 ;.col-md-4.col-md-offset-4
         [:button.btn.btn-primary.btn-block
          {:on-click (fn [_]
                       (let [turn (gossip/random-turn db)]
                         (swap! app-state assoc
                                :encounter (narr/narrate-turn turn))))
           }
          "New encounter"
          ]]])
     (when (:encounter @app-state)
       [:div.row
        [:div.col-lg-12 ;.col-md-4.col-md-offset-4
         [:button.btn.btn-success.btn-block
          {:on-click (fn [_]
                       (if-let [enc (:encounter @app-state)]
                         (swap! app-state assoc
                                :db (:db enc)
                                :encounter nil)))
           }
          "Continue"
          ]]])
     (when-let [enc (:encounter @app-state)]
       (let [ini (name (:initiator enc))
             par (name (:partner enc))
             ini-ava (:person/avatar
                      (d/pull db '[*] [:person/id (:initiator enc)]))
             par-ava (:person/avatar
                      (d/pull db '[*] [:person/id (:partner enc)]))]
         [:div
          [:div ;.col-md-4
           [:p.lead
            (:meet-phrase enc)]
           [:div
            {:style {:float "left"
                     :padding "1em"
                     :margin "0 1em 1em 0"
                     :background "#eee"
                     :border "1px solid #ddd"
                     :border-radius "33%"}}
            [:div.text-center {:style {:font-size "5em"}}
             ini-ava]
            [:h4.text-center ini]
            ]
           (turn-part-pane (:fwd-part enc)
                           (:fwd-thoughts enc)
                           (:fwd-reply enc))
           [:div
            {:style {:float "right"
                     :padding "1em"
                     :margin "0 0 1em 1em"
                     :background "#eee"
                     :border "1px solid #ddd"
                     :border-radius "33%"}}
            [:div.text-center {:style {:font-size "5em"}}
             par-ava]
            [:h4.text-center par]
            ]
           (turn-part-pane (:back-part enc)
                           (:back-thoughts enc)
                           (:back-reply enc))
           (when-let [thoughts (seq (:partner-thoughts enc))]
             [:p
              [:i
               [:b (str par " thinks: ")]
               (->> thoughts
                    (map :belief/phrase)
                    (str/join \newline))]
              ])
           (when-let [thoughts (seq (:initiator-thoughts enc))]
             [:p
              [:i
               [:b (str ini " thinks: ")]
               (->> thoughts
                    (map :belief/phrase)
                    (str/join \newline))]])
           (let [part (:fwd-part enc)]
             )
           [:p (str "Fwd thoughts: " (:fwd-thoughts enc))]
           [:p (str "Back thoughts: " (:back-thoughts enc))]
           [:p (str (:partner-thoughts enc))]
           [:p (str (:initiator-thoughts enc))]
           ]
          ]))

     ]))

(defn navbar
  [app-state]
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
         :on-click #()
         :title "Step backward in time"
         :disabled "disabled"}
        [:span.glyphicon.glyphicon-step-backward {:aria-hidden "true"}]
        [:span.visible-xs-inline " Step backward"]]]
      ;; step forward
      [:li
       [:button.btn.btn-default.navbar-btn
        {:type :button
         :on-click #()
         :title "Step forward in time"
         :disabled "disabled"}
        [:span.glyphicon.glyphicon-step-forward {:aria-hidden "true"}]
        [:span.visible-xs-inline " Step forward"]]]]]]])

(defn app-pane
  [app-state]
  [:div
   [navbar app-state]
   [:div.container-fluid
    [:div.row
     [:div.col-lg-6
      [add-person-pane app-state]]
     [:div.col-lg-6
      [add-belief-pane app-state]]]
    [:div.row
     [:div.col-lg-8.col-md-6
      [status-pane app-state]]
     [:div.col-lg-4.col-md-6
      [encounter-pane app-state]]]
    ]
   ])

(reagent/render-component [app-pane app-state]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
