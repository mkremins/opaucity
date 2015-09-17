(ns opaucity.app
  (:require [om.core :as om]
            [om-tools.core :refer-macros [defcomponent defcomponentmethod]]
            [om-tools.dom :as dom]))

;; utils

(enable-console-print!)

(defn dissoc-in [m path]
  (update-in m (pop path) dissoc (peek path)))

(defn ev->pos [ev]
  {:x (.-clientX ev) :y (.-clientY ev)})

;; model

(defonce app-state
  (atom {:things {0 {:x 0 :y 0 :type :number :value 2}
                  1 {:x 20 :y 20 :type :number :value 3}
                  2 {:x 40 :y 40 :type :adder :slots [nil nil]}}}))

(defn get-thing [state id]
  (get-in state [:things id]))

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

(defmethod simplify :adder [state id]
  (let [{:keys [slots x y]} (get-thing state id)
        [a b] (map (comp :value (partial get-thing state)) slots)]
    (-> (reduce #(dissoc-in %1 [:things %2]) state (conj slots id))
        (add-thing {:x x :y y :type :number :value (+ a b)}))))

(defn maybe-simplify [state id]
  (let [{:keys [slots]} (get-thing state id)]
    (cond-> state (and slots (every? identity slots)) (simplify id))))

(defn put-thing-in-slot [state id [parent-id slot]]
  (-> state
      (remove-thing-from-containing-slot id)
      (assoc-in [:things parent-id :slots slot] id)
      (assoc-in [:things id :slot] [parent-id slot])
      (maybe-simplify parent-id)))

(defn move-thing-to-position [state id pos]
  (-> state
      (remove-thing-from-containing-slot id)
      (update-in [:things id] merge pos)))

(defn swap-things [state id1 id2]
  (let [{slot1 :slot, x1 :x, y1 :y} (get-thing state id1)
        {slot2 :slot, x2 :x, y2 :y} (get-thing state id2)]
    (-> state
        (remove-thing-from-containing-slot id1)
        (remove-thing-from-containing-slot id2)
        (cond-> slot1       (put-thing-in-slot id2 slot1)
                (not slot1) (move-thing-to-position id2 {:x x1 :y y1})
                slot2       (put-thing-in-slot id1 slot2)
                (not slot2) (move-thing-to-position id1 {:x x2 :y y2})))))

(defn handle-click [state kind info]
  (prn kind info state)
  (if-let [selected (:selected state)]
    (-> (case kind
          :pos (move-thing-to-position state selected info)
          :slot (put-thing-in-slot state selected info)
          :thing (swap-things state selected info))
        (dissoc :selected))
    (cond-> state (= kind :thing) (assoc :selected info))))

;; Om components

(defmulti thing-view
  (fn [data _] (:type (get-thing data (:id data)))))

(defcomponentmethod thing-view :number [data owner]
  (render [_]
    (let [id (:id data)
          thing (get-thing data id)
          x (if (:slot thing) 0 (:x thing))
          y (if (:slot thing) 0 (:y thing))
          selected? (= id (:selected data))]
      (dom/div {:class (cond-> "thing number" selected? (str " selected"))
                :on-click
                (fn [ev]
                  (.stopPropagation ev)
                  (om/transact! data #(handle-click % :thing id)))
                :style {:left (str x "px") :top (str y "px")}}
        (:value thing)))))

(defcomponentmethod thing-view :adder [data owner]
  (render [_]
    (let [id (:id data)
          thing (get-thing data id)
          x (if (:slot thing) 0 (:x thing))
          y (if (:slot thing) 0 (:y thing))
          selected? (= id (:selected data))
          [child-id1 child-id2] (:slots thing)]
      (dom/div {:class (cond-> "thing adder" selected? (str " selected"))
                :on-click
                (fn [ev]
                  (.stopPropagation ev)
                  (om/transact! data #(handle-click % :thing id)))
                :style {:left (str x "px") :top (str y "px")}}
        (dom/div {:class "slot"
                  :on-click
                  (fn [ev]
                    (.stopPropagation ev)
                    (om/transact! data #(handle-click % :slot [id 0])))}
          (when child-id1
            (om/build thing-view (assoc data :id child-id1))))
        "+"
        (dom/div {:class "slot"
                  :on-click
                  (fn [ev]
                    (.stopPropagation ev)
                    (om/transact! data #(handle-click % :slot [id 1])))}
          (when child-id2
            (om/build thing-view (assoc data :id child-id2))))))))

(defcomponent app [data owner]
  (render [_]
    (dom/div {:on-click
              (fn [ev]
                (.stopPropagation ev)
                (om/transact! data #(handle-click % :pos (ev->pos ev))))}
      (for [id (keys (:things data))
            :when (not (:slot (get-thing data id)))]
        (om/build thing-view (assoc data :id id))))))

(om/root app app-state {:target (js/document.getElementById "app")})
