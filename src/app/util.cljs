(ns app.util
  (:require [goog.object :as go]))


(defn get-touch-xy [^js evt]
  (let [touch-list (.-targetTouches evt)]
    (if (pos? (.-length touch-list))
      (let [touch-item (.item touch-list 0)]
        [(.-clientX touch-item) (.-clientY touch-item)])
      [0 0])))


(defn get-mouse-xy [^js evt]
  [(.-clientX evt) (.-clientY evt)])


(defn evt-inside? [^js evt {:keys [x1 y1 x2 y2]}]
  (let [[x y] (if (-> evt .-type (= "touchmove"))
                (get-touch-xy evt)
                (get-mouse-xy evt))]
    (and (<= x1 x x2)
         (<= y1 y y2))))


(defn prevent-default [^js evt]
  (.preventDefault evt)
  evt)


(defn make-timer-evt []
  (doto (js/Object.)
    (go/set "type" "timer")
    (go/set "preventDefault" identity)))


(defn client-rect->bounds [^js client-rect]
  {:x1 (.-left client-rect)
   :x2 (.-right client-rect)
   :y1 (.-top client-rect)
   :y2 (.-bottom client-rect)})


(defn get-target-coords [^js target]
  (let [dataset (-> target .-dataset)]
    [(-> dataset .-x (js/parseInt 10))
     (-> dataset .-y (js/parseInt 10))]))
