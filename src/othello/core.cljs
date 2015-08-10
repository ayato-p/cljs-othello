(ns othello.core
  (:require-macros [reagent.ratom :refer [reaction]]
                   [cljs.core.match :refer [match]])
  (:require [clojure.string :as str]
            [clojure.browser.dom :as dom]
            [cljs.core.match]
            [reagent.core :as r :refer [atom]]
            [re-frame.core :refer [dispatch
                                   dispatch-sync
                                   after
                                   path
                                   register-handler
                                   register-sub
                                   subscribe]]))

(def initial-state
  {:turn true
   :players [{:name "ayato_p"}
             {:name "alea12"}]
   :disks (-> (vec (repeat 8 (vec (repeat 8 -1))))
              (assoc-in [3 3] 1)
              (assoc-in [4 3] 0)
              (assoc-in [3 4] 0)
              (assoc-in [4 4] 1))
   :error false})

(defn get-around [disks x y]
  (map #(vector % (get-in disks %))
       (for [x' [-1 0 1]
             y' [-1 0 1]
             :when (not= 0 x' y')]
         [(+ x x') (+ y y')])))

(defn take-valid-coordinates [values v]
  (into []
        (comp (filter (fn [[_ v']] (and v' (not= v' -1) (not= v v'))))
              (map first))
        values))

(defn collect-disks [disks [x y :as from] [x' y' :as to] v]
  (let [[x+ y+ :as direction] [(- x' x) (- y' y)]]
    (loop [[nx ny :as next] to r []]
      (let [v' (get-in disks next)]
        (cond
          (or (nil? v') (= v' -1)) []
          (= v v') r
          :else (recur [(+ nx x+) (+ ny y+)] (conj r next)))))))

(defn remove-nil+empty-seq [coll]
  (seq (remove (comp nil? not-empty) coll)))

(defn reverse-targets [disks x y v]
  (let [around (get-around disks x y)
        coordinates (take-valid-coordinates around v)]
    (remove-nil+empty-seq (map #(collect-disks disks [x y] % v) coordinates))))

(defn can-put? [disks x y v]
  (let [target (get-in disks [x y])
        around-values (map second (get-around disks x y))]
    (and (= target -1)
         (some (hash-set (if (zero? v) 1 0)) around-values)
         (reverse-targets disks x y v))))

;;; Event handlers

(register-handler
 ::initialize
 (fn [db _]
   (merge db initial-state)))

(register-handler
 ::put-disk
 (comp (path [:disks])
       (after (fn [_ x y v] (dispatch [::reverse x y v]))))
 (fn [disks [_ x y v]]
   (dispatch [::clear])
   (dispatch [::next-turn])
   (assoc-in disks [x y] v)))

(register-handler
 ::reverse
 (path [:disks])
 (fn [disks [_ [_ x y v]]]
   (let [targets (apply concat (reverse-targets disks x y v))]
     (reduce #(assoc-in %1 %2 v) disks targets))))

(register-handler
 ::next-turn
 (path [:turn])
 (fn [turn _]
   (not turn)))

(register-handler
 ::error
 (path [:error])
 (fn [error _] true))

(register-handler
 ::clear
 (path [:error])
 (fn [error _] false))

;;; Subscription handlers

(register-sub
 ::turn
 (fn [db _]
   (reaction (:turn @db))))

(register-sub
 ::players
 (fn [db _]
   (reaction (:players @db))))

(register-sub
 ::disks
 (fn [db _]
   (reaction (:disks @db))))

(register-sub
 ::error
 (fn [db _]
   (reaction (:error @db))))

(register-sub
 ::scores
 (fn [db _]
   (let [disks (flatten (:disks @db))
         disks-group (dissoc (group-by identity disks) -1)]
     (reaction (mapv #(count (disks-group %)) (range 2))))))

;;; Component

(defn disk [x y v]
  [:circle {:cx (+ x 0.5)
            :cy (+ y 0.5)
            :r :0.45
            :fill (if (zero? v) :black :white)}])

(defn piece [x y turn]
  (let [disks (subscribe [::disks])
        v (if turn 0 1)]
    [:rect {:x x
            :y y
            :width 1
            :height 1
            :stroke :black
            :stroke-width 0.01
            :fill :none
            :on-click #(if (can-put? @disks x y v)
                         (dispatch [::put-disk x y v])
                         (dispatch [::error]))}]))

(defn board []
  (let [disks (subscribe [::disks])
        turn (subscribe [::turn])]
    [:div
     [:svg {:style {:width 400
                    :height 400
                    :border "1px solid black"
                    :pointer-events :visible
                    :background :green}
            :view-box (str/join " " [0 0 8 8])}

      (into [:g]
            (for [x (range 8) y (range 8)]
              (let [v (get-in @disks [x y])]
                (when (>= v 0) [disk x y v]))))

      (into [:g]
            (for [x (range 8) y (range 8)]
              [piece x y @turn]))]]))

(defn world []
  (let [players (subscribe [::players])
        turn (subscribe [::turn])
        error (subscribe [::error])
        scores (subscribe [::scores])]
    [:div
     [:dl
      [:dt (str/join ", " (map #(str (:name %1) "'s score: " %2) @players @scores))]
      [:dd (str "Next person: " (:name ((if @turn first second) @players)))]]
     (when @error
       [:span {:style {:color :red}} "ERROR: You can not put there!!"])
     [board]
     [:button {:on-click #(dispatch [::next-turn])} "Pass"]
     [:button {:on-click #(dispatch-sync [::initialize])} "Reset"]]))

(defn main []
  (when-let [elm (dom/get-element "app")]
    (dispatch-sync [::initialize])
    (r/render [world] elm)))

(main)
