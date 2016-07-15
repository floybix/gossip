(ns gossip.app
  (:require [reagent.core :as reagent :refer [atom]]
            [goog.dom.forms :as forms]
            [datascript.core :as d]
            [gossip.core :as gossip]
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
      {:on-click #(let [s (str/replace (:name person) " " "_")
                        id (keyword s)
                        gender (if (:male? person)
                                 :male :female)]
                    (swap! app-state update :db
                           d/db-with [{:person/id id
                                       :person/gender gender}])
                    (swap! app-state assoc-in [:adding-person :name] "")
                    )
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
                       (println "subj" s)
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
                       (println "feeling" s)
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
                       (println "obj s")
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
      {:on-click #(let [{:keys [subject object feeling]} belief
                        ;subject (keyword subject)
                        ;object (keyword object)
                        ;feeling (keyword feeling)
                        ]
                    (println belief)
                    (swap! app-state update :db
                           gossip/believe subject subject object feeling)
                    (swap! app-state assoc-in [:adding-belief :object] nil)
                    )
       :disabled (when-not (and (:subject belief)
                                (:object belief))
                   "disabled")}
      "Add belief"]
     ])
  )

(defn status-pane
  [app-state]
    (let [db (:db @app-state)
        people (gossip/all-people db)]
    [:div
     (into [:div.row]
           (for [person people]
             [:div.col.xs-6.sm-3.col-lg-2
              [:h4 (name person)]
              (let [by-subj (->> (d/q gossip/feelings-q db person)
                                 (sort-by :relation/object)
                                 (group-by :relation/subject))
                    ]
                [:div
                 (into [:ul.list-unstyled]
                       (for [belief (get by-subj person)]
                         [:li
                          (gossip/phrase-belief db belief person nil)]))
                 (into [:div]
                       (for [[subj bs] (dissoc by-subj person)]
                         [:div
                          [:h5 (name subj)]
                          (into [:ul.list-unstyled]
                                (for [belief bs]
                                  [:li
                                   (gossip/phrase-belief db belief person nil)]))]
                         ))])
              ]))
     ]))

(defn encounter-pane
  [app-status]
  (let [db (:db @app-state)
        people (gossip/all-people db)]
    [:div
     [:div.row
      (when (and (>= (count people) 2)
                 (not (:encounter @app-state)))
        [:div.col-md-4.col-md-offset-4
         [:button.btn.btn-primary.btn-block
          {:on-click (fn [e]
                       (if-let [ans (->> (repeatedly 100 #(gossip/step db))
                                         (remove nil?)
                                         (first))]
                         (swap! app-state assoc
                                :encounter ans)
                         ;; no news!
                         nil))
           }
          "New encounter"
          ]])
      (when (:encounter @app-state)
        [:div.col-md-4.col-md-offset-4
         [:button.btn.btn-success.btn-block
          {:on-click (fn [e]
                       (if-let [enc (:encounter @app-state)]
                         (swap! app-state assoc
                                :db (:db enc)
                                :encounter nil)))
           }
          "Continue"
          ]])]
     [:div.row
      [:div.col-lg-6
       (str (:encounter @app-state))]]

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
    [status-pane app-state]
    [encounter-pane app-state]
    ]
   ])

(reagent/render-component [app-pane app-state]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
