(ns opaucity.app
  (:require [om.core :as om]
            [om-tools.core :refer-macros [defcomponent defcomponentmethod]]
            [om-tools.dom :as dom]))

;; utils

(enable-console-print!)

(defn dissoc-in [m path]
  (update-in m (pop path) dissoc (peek path)))

(defn ev->pos [ev]
  [(.-clientX ev) (.-clientY ev)])

;; model

(defn get-thing [state id]
  (get-in state [:things id]))

(defn get-the-thing
  "Gets the thing to render. For use by Om components that represent things."
  [state]
  (get-thing state (:id state)))

(defn add-thing [state thing]
  (let [id (inc (apply max (conj (keys (:things state)) -1)))]
    (assoc-in state [:things id] thing)))

(defn remove-thing-from-containing-slot [state id]
  (if-let [[parent-id slot] (:slot (get-thing state id))]
    (-> state
        (assoc-in [:things parent-id :slots slot] nil)
        (dissoc-in [:things id :slot]))
    state))

(defmulti simplify
  (fn [state id] (:type (get-thing state id))))

(def get-binop
  {"+" + "-" - "*" * "/" /})

(defmethod simplify :binop [state id]
  (let [{:keys [name pos slots]} (get-thing state id)
        [a b] (map (comp :value (partial get-thing state)) slots)
        op (get-binop name)]
    (-> (reduce #(dissoc-in %1 [:things %2]) state (conj slots id))
        (add-thing {:pos pos :type :number :value (op a b)}))))

(defn maybe-simplify [state id]
  (let [{:keys [slots]} (get-thing state id)]
    (cond-> state (and slots (every? identity slots)) (simplify id))))

(defn can-put-thing-in-slot? [state id [parent-id slot]]
  (and (not= id parent-id) ; can't put a thing into one of its own slots
       (let [thing (get-thing state id)
             parent (get-thing state parent-id)]
         (case (:type parent)
           :binop (= (:type thing) :number)
           :goal (= (:value thing) (:value parent))))))

(defn put-thing-in-slot [state id [parent-id slot]]
  (if (can-put-thing-in-slot? state id [parent-id slot])
    (-> state
        (remove-thing-from-containing-slot id)
        (assoc-in [:things parent-id :slots slot] id)
        (assoc-in [:things id :slot] [parent-id slot])
        (maybe-simplify parent-id))
    state))

(defn move-thing-to-position [state id pos]
  (-> state
      (remove-thing-from-containing-slot id)
      (assoc-in [:things id :pos] pos)))

(defn swap-things [state id1 id2]
  (let [{slot1 :slot, pos1 :pos} (get-thing state id1)
        {slot2 :slot, pos2 :pos} (get-thing state id2)]
    (-> state
        (remove-thing-from-containing-slot id1)
        (remove-thing-from-containing-slot id2)
        (cond-> slot1       (put-thing-in-slot id2 slot1)
                (not slot1) (move-thing-to-position id2 pos1)
                slot2       (put-thing-in-slot id1 slot2)
                (not slot2) (move-thing-to-position id1 pos2)))))

(defn handle-click [state kind info]
  (prn kind info state)
  (if-let [selected (:selected state)]
    (-> (case kind
          :pos (move-thing-to-position state selected info)
          :slot (put-thing-in-slot state selected info)
          :thing (swap-things state selected info))
        (dissoc :selected))
    (cond-> state (= kind :thing) (assoc :selected info))))

(def levels
  [; level 0
   [{:pos [0 0] :type :number :value 2}
    {:pos [60 0] :type :number :value 2}
    {:pos [0 60] :type :binop :name "+" :slots [nil nil]}
    {:pos [200 0] :type :goal :value 4 :slots [nil]}]
   ; level 1
   (into (map #(-> {:pos [(* % 60) 0] :type :number :value %}) (range 5))
         [{:pos [0 60] :type :binop :name "+" :slots [nil nil]}
          {:pos [0 140] :type :binop :name "-" :slots [nil nil]}
          {:pos [0 220] :type :binop :name "*" :slots [nil nil]}
          {:pos [0 300] :type :binop :name "/" :slots [nil nil]}])])

(defn switch-to-level [state level]
  (reduce add-thing {:things {} :level level} (nth levels level)))

(defmethod simplify :goal [state id]
  (switch-to-level state (inc (:level state))))

;; app state

(defonce app-state
  (atom (switch-to-level {} 0)))

;; Om components

(defmulti thing-view
  (fn [data _] (:type (get-thing data (:id data)))))

(defcomponent thing-wrapper [data owner]
  (render [_]
    (let [id (:id data)
          thing (get-thing data id)
          [x y] (if (:slot thing) [0 0] (:pos thing))
          selected? (= id (:selected data))]
      (dom/div {:class (cond-> (str "thing " (name (:type thing)))
                               selected? (str " selected"))
                :on-click (fn [ev]
                            (.stopPropagation ev)
                            (om/transact! data #(handle-click % :thing id)))
                :style {:left (str x "px") :top (str y "px")}}
        (om/build thing-view data)))))

(defcomponent slot-view [data owner]
  (render [_]
    (let [{:keys [id slot-id selected]} data
          valid-target? (can-put-thing-in-slot? data selected [id slot-id])]
      (dom/div {:class (cond-> "slot" valid-target? (str " valid-target"))
                :on-click (fn [ev]
                            (.stopPropagation ev)
                            (om/transact! data #(handle-click % :slot [id slot-id])))}
        (when-let [child-id (get-in (get-thing data id) [:slots slot-id])]
          (om/build thing-wrapper (assoc data :id child-id)))))))

(defcomponentmethod thing-view :number [data owner]
  (render [_]
    (dom/span (:value (get-the-thing data)))))

(defcomponentmethod thing-view :goal [data owner]
  (render [_]
    (dom/div
      (dom/p {:class "explanation"}
        "Give me " (dom/code (pr-str (:value (get-the-thing data)))) "!")
      (om/build slot-view (assoc data :slot-id 0)))))

(defcomponentmethod thing-view :binop [data owner]
  (render [_]
    (dom/div
      (om/build slot-view (assoc data :slot-id 0))
      (:name (get-the-thing data))
      (om/build slot-view (assoc data :slot-id 1)))))

(defcomponent level-selector [data owner]
  (render [_]
    (dom/div {:class "level-selector"}
      (dom/span "Level:")
      (for [level (range (count levels))]
        (if (= (:level data) level)
          (dom/strong level)
          (dom/a {:href "#"
                  :on-click
                  (fn [ev]
                    (.preventDefault ev)
                    (om/transact! data #(switch-to-level % level)))}
            level))))))

(defcomponent app [data owner]
  (render [_]
    (dom/div
      (om/build level-selector data)
      (dom/div {:class "board"
                :on-click
                (fn [ev]
                  (.stopPropagation ev)
                  (om/transact! data #(handle-click % :pos (ev->pos ev))))}
        (for [id (keys (:things data))
              :when (not (:slot (get-thing data id)))]
          (om/build thing-wrapper (assoc data :id id)))))))

(om/root app app-state {:target (js/document.getElementById "app")})
