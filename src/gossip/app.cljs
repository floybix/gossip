(ns gossip.app
  (:require [reagent.core :as reagent :refer [atom]]
            [goog.dom.forms :as forms]
            [datascript.core :as d]
            [gossip.core :as gossip]
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
         :on-change (fn [e]
                      (swap! app-state update-in
                             [:adding-person :male?]
                             not))}]
       "male"]]
     ;; add button
     [:button.btn.btn-default
      {:on-click
       (fn [e]
         (let [s (str/replace (:name person) " " "_")
               id (keyword s)
               gender (if (:male? person)
                        :male :female)]
           (swap! app-state update :db
                  d/db-with [{:person/id id
                              :person/gender gender}])
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
       (fn [e]
         (let [{:keys [subject object feeling]} belief]
           (swap! app-state update :db
                  gossip/believe subject subject subject object feeling)
           (swap! app-state assoc-in [:adding-belief :object] nil)))
       :disabled (when-not (and (:subject belief)
                                (:object belief))
                   "disabled")}
      "Add belief"]]))

(defn status-pane
  [app-state]
  (let [db (:db @app-state)
        pov (:current-pov @app-state)
        people (gossip/all-people db)
        belief-li (fn [mind belief]
                    (let [b-mind (:belief/mind belief)
                          b-person (:belief/person belief) ; either pov or mind
                          subj (:belief/subject belief)
                          ;; wait, are we checking the feeling or checking the belief?
                          check? (if (= b-person b-mind subj)
                                   false true)]
                      [:li
                       {:class (if (:missing? belief)
                                 "text-muted"
                                 (case (:belief/feeling belief)
                                   :like "bg-warning" ;; yellow
                                   :anger "bg-danger" ;; red
                                   :fear "bg-info" ;; blue
                                   ))}
                       [(if (:missing? belief) :del :span)
                        (gossip/phrase-belief db belief (:belief/mind belief) nil)]]))]
    [:div
     [:div.row
      [:div.col-lg-12
       (if pov
         [:p "Showing what " [:b (name pov)] " knows. "
          [:button.btn-primary.btn-xs
           {:on-click
            (fn [e]
              (swap! app-state assoc :current-pov nil))}
           "Show true feelings"]]
         [:p.text-muted
          "Select one person to show what they know:"])]]
     (into [:div.row]
           (for [mind people]
             [:div.col-xs-6.col-sm-4.col-md-3.col-lg-2
              [:div.panel
               {:class (if (= mind pov) "panel-primary" "panel-default")}
               [:div.panel-heading
                [:h4.panel-title
                 (if (= mind pov)
                   (name mind)
                   [:a
                    {:href "#"
                     :on-click
                     (fn [e]
                       (swap! app-state assoc :current-pov mind))}
                    (name mind)])
                 (if (and pov (not= mind pov))
                   [:small (str " according to " (name pov))])]
                ]
               [:div.panel-body
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
                      "I don't know others' feelings."])])]
               [:div.panel-footer
                (let [pops (d/q gossip/perceived-popularity-q
                                db (or pov mind) mind)]
                  (str (sort > pops))
                  ;; who is most popular
                  ;; who am i indebted to
                  )]]
              ]))]))

(defn turn-part-pane
  [part thoughts reply]
  (let [{:keys [speaker listener db gossip minor-news]} part
        spe (name speaker)
        lis (name listener)]
    [:div
     [:p
      [:b (str spe ": ")]
      (str \"
           (if gossip
             (gossip/phrase-gossip db speaker listener gossip)
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
      (str \" reply \")]
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
          {:on-click (fn [e]
                       (let [ans (gossip/random-turn db)]
                         (swap! app-state assoc
                                :encounter ans)))
           }
          "New encounter"
          ]]])
     (when (:encounter @app-state)
       [:div.row
        [:div.col-lg-12 ;.col-md-4.col-md-offset-4
         [:button.btn.btn-success.btn-block
          {:on-click (fn [e]
                       (if-let [enc (:encounter @app-state)]
                         (swap! app-state assoc
                                :db (:db enc)
                                :encounter nil)))
           }
          "Continue"
          ]]])
     (when-let [enc (:encounter @app-state)]
       (let [ini (name (:initiator enc))
             par (name (:partner enc))]
         [:div
          [:div ;.col-md-4
           [:p.lead
            (:meet-phrase enc)]
           [:div
            {:style {:float "left"
                     :padding "1em"
                     :margin "1em"
                     :border "1px solid black"}}
            [:h2.text-center ini]
            ]
           (turn-part-pane (:fwd-part enc)
                           (:fwd-thoughts enc)
                           (:fwd-reply enc))
           [:div
            {:style {:float "right"
                     :padding "1em"
                     :margin "1em"
                     :border "1px solid black"}}
            [:h2.text-center par]
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
           [:p (str (:gossip (:fwd-part enc)))]
           [:p (str (:gossip (:back-part enc)))]
           [:p (str (:fwd-thoughts enc))]
           [:p (str (:back-thoughts enc))]
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
