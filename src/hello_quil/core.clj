(ns hello-quil.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clojure.core.reducers :as r]))

(def scaler 50.0)
(defstruct cell :index :alive :x :y)
(defn scale-w [] (Math/floor (/ (q/screen-width) scaler) ))
(defn scale-h [] (Math/floor (/ (q/screen-height) scaler) ))


(defn scaledCoord [coord] (if (= coord 0) coord (* scaler coord)))

(def initState
  ; the initial state 
  (map-indexed 
    (fn [index num]
      (struct cell index (rand-int 2) (scaledCoord (mod index (scale-w))) 
        (scaledCoord(Math/floor (/ index (scale-w)))))) 
    (range 0 (* (scale-w) (scale-h))))
)

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 15)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  ; setup function returns initial state. It contains
  {:cells (into [] initState)}
)

(defn getNeighbor [x y cells]
  ; gets the neigbor or return a random alive if no neighbor 
  (let [result (into [] (r/filter (fn [c] (and (= x (:x c)) (= y (:y c)))) cells))] 
  (if (empty? result) (rand-int 2) (:alive (first result))))
)


(defn getNeighbors [c cells]
  ; get neighbors in 8 directions
  (let [x (:x c) y (:y c) left (- x scaler) right (+ x scaler) up (+ y scaler) down (- y scaler)]
    (map 
      (fn [[newX newY]](getNeighbor newX newY cells)) 
      [[right y] [left y] [right up] [left up] [x up] [x down] [right down] [left down]]))
)


(defmulti cell-state
  ; takes a vector of neighbors and sums them
  (fn [c cells] (reduce + cells))
)
; if there are 2 the cell stays the same
(defmethod cell-state 2 [c cells] c)
; if there are 3 neighbors we reproduce 1
(defmethod cell-state 3 [c _] (assoc c :alive 1))
; everything else it dies
(defmethod cell-state :default [c _] (assoc c :alive 0))

(defn update-cell [cells]
  ; curried func that takes the state, then return a mappable func
  (fn [c] (let [neighbors (getNeighbors c cells)]
    (cell-state c neighbors)))
)

(defn conway-state [cells]
  ; update conway's game of life
  (doall (r/reduce concat (r/map #(map (update-cell cells) %) (partition-all 200 cells))) )
)

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  {:cells (conway-state (:cells state))} 
)

(defn getFill [alive] 
  (if (> alive 0) 255 0)
)

(defn draw-cell [c] 
  (q/with-fill [(getFill (:alive c))] (q/rect (:x c) (:y c) scaler scaler))
)

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
  (q/fill (q/random 255))
  ; Calculate x and y coordinates of the cell and the color.
  (doseq [c (:cells state)] (draw-cell c))
  ;(doall (pmap #(map draw-cell %) (partition 4 (:cells state))))
  )

(q/defsketch hello-quil
  :title "Basic Conway Game of Life"
  :size [(q/screen-width) (q/screen-height)]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])