(ns gossip.viz
  (:require [datascript.core :as d]
            [reagent.core :as reagent :refer [atom]]
            [gossip.core :as gossip]
            [gossip.narrative :as narr]
            [clojure.set :as set]
            [clojure.string :as str]))


(defn interp
  [from to frac offset]
  (let [[fx fy] from
        [tx ty] to
        dx (- tx fx)
        dy (- ty fy)
        dist (Math/sqrt (+ (* dx dx) (* dy dy)))
        nx (int (+ fx (* frac dx)))
        ny (int (+ fy (* frac dy)))]
    ;; add the offset distance along the line
    [(int (+ nx (* dx (/ offset dist))))
     (int (+ ny (* dy (/ offset dist))))]))

(defn dxdy
  [dragging]
  (let [{[ox oy] :from, [x y] :at} dragging
        dx (- x ox)
        dy (- y oy)]
    [dx dy]))

(def PERSON_RADIUS 100)

(def dragging (atom nil))

(defn social-graph-svg
  [ui-state db pov]
  (let [people (gossip/all-people db)
        ppl-at (:graph-coords @ui-state)
        drag-move (fn [e]
                    ;(.preventDefault e)
                    (when @dragging
                      (let [x (or (.-clientX e)
                                  (-> e .-changedTouches (aget 0) .-clientX))
                            y (or (.-clientY e)
                                  (-> e .-changedTouches (aget 0) .-clientY))
                            [dx dy] (dxdy @dragging)
                            mind (:mind @dragging)
                            [my-x my-y] (:original @dragging)]
                        (swap! dragging assoc :at [x y])
                        (swap! ui-state assoc-in [:graph-coords mind]
                               [(+ my-x dx) (+ my-y dy)]))))
        drag-end (fn [e]
                   (when @dragging
                     (reset! dragging nil)))]
    [:svg
     {;:width "100%"
      ;:height "50%"
      :id "gossip-graph"
      :style {:width "100%"
              :height "50vh"}
      :onMouseMove drag-move
      :onTouchMove drag-move
      :onMouseUp drag-end
      :onTouchEnd drag-end}
     (into
      [:g]
      (for [mind people
            :let [avatar (get-in @ui-state [:avatars mind] "@")
                  [my-x my-y] (get ppl-at mind [100 100])
                  drag-start (fn [e]
                               (.preventDefault e)
                               (let [x (or (.-clientX e)
                                           (-> e .-changedTouches (aget 0) .-clientX))
                                     y (or (.-clientY e)
                                           (-> e .-changedTouches (aget 0) .-clientY))]
                                 (reset! dragging {:from [x y]
                                                   :at [x y]
                                                   :original [my-x my-y]
                                                   :mind mind})))]]
        [:g
         [:ellipse.draggable
          {:key mind
           :cx my-x
           :cy my-y
           :rx (quot PERSON_RADIUS 2)
           :ry (quot PERSON_RADIUS 2)
           :fill "#eee"
           :stroke (if (= pov mind) "#337ab7" "#ddd")
           :stroke-width (if (= pov mind) "3" "1")
           :onMouseDown drag-start
           :onTouchStart drag-start
           }]
         [:text.player-name
          {:text-anchor "middle"
           :x my-x
           :y (+ my-y (quot PERSON_RADIUS 2) -16)
           :style {:font-size "16px"
                   :font-weight "500"
                   :cursor "pointer"}
           :on-click
           (fn [_]
             (swap! ui-state assoc :current-pov mind))}
          (name mind)]
         [:text.player-avatar.draggable
          {:text-anchor "middle"
           :x my-x
           :y (+ my-y (* PERSON_RADIUS 0.1))
           :style {:font-size (str (int (* PERSON_RADIUS 0.5)) "px")}
           :onMouseDown drag-start
           :onTouchStart drag-start}
          avatar]]))
     (into
      [:g]
      (for [mind people
            :let [knowl (d/q gossip/my-knowledge-of-their-beliefs-q
                             db (or pov mind) mind)]
            belief knowl
            :let [subj (:belief/subject belief)
                  obj (:belief/object belief)]
            ;; only show beliefs where this mind is the subject or object
            :when (or (= mind subj)
                      (= mind obj))
            :let [mind-xy (get ppl-at mind)
                  them (if (= mind subj) obj subj)
                  them-xy (get ppl-at them)
                  from-xy (if (= mind subj)
                            (interp mind-xy them-xy 0.01 (+ 8 (quot PERSON_RADIUS 2)))
                            (interp mind-xy them-xy 0.5 -16))
                  to-xy (if (= mind obj)
                          (interp mind-xy them-xy 0.01 (+ 8 (quot PERSON_RADIUS 2)))
                          (interp mind-xy them-xy 0.5 -16))
                  symbol (case (:belief/feeling belief)
                           :like "😍"
                           :fear "😨"
                           :anger "😡"
                           :none "⚪")
                  modif (if (and (:belief/lie? belief)
                                 (or (not pov)
                                     (= pov (:belief/fabricator belief))))
                          "❌" "")]]
        [:g
         [:polyline
          {:points (str/join " " (concat from-xy to-xy))
           :stroke "#000"
           :stroke-opacity "0.2"
           :stroke-width "2"}]
         [:g
          [:text
           {:text-anchor "middle"
            :x (first to-xy)
            :y (+ (second to-xy) 8)
            :style {:font-size "16px"
                    :cursor "default"}
            }
           (str symbol modif)]
          [:title (narr/belief-explanation db pov belief)]]]
        ))
     ]
    ))
