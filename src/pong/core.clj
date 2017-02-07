(ns pong.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 120)
  (q/no-cursor)
  (let [player-w (/ (q/width) 150)
        player-h (/ (q/height) 10)
        ball-w (/ (q/width) 150)]
    {:p1-score 0
     :p2-score 0
     :p-w player-w
     :p-h player-h
     :p1-x 0
     :p2-x (- (q/width) player-w)
     :p1-y (/ (q/height) 2)
     :p2-y (/ (q/height) 2)
     :ball-x (* 2 player-w)
     :ball-y 0
     :ball-w ball-w
     :ball-h ball-w
     :iball-vx (/ player-w 2.0)
     :iball-vy (/ player-w 2.0)
     :ball-vx (/ player-w 2.0)
     :ball-vy (/ player-w 2.0)
     :p1-up 0
     :p2-up 0
     :p1-down 0
     :p2-down 0}))

(defn collision-x [state bx by p1-y p2-y]
  (let [p-w (state :p-w)
        p-h (state :p-h)
        p1-x (state :p1-x)
        p2-x (state :p2-x)]
    (cond
      (and (>= bx p1-x) (<= bx (+ p1-x p-w)) (>= by p1-y) (<= by (+ p-h p1-y))) -1 ; inside p1
      (and (>= bx p2-x) (<= bx (+ p2-x p-w)) (>= by p2-y) (<= by (+ p-h p2-y))) -1 ; inside p2
      :else 1)))

(defn collision-y [by]
  (if (or (< by 0) (> by (q/height))) ;; outside field
    -1
    1))

(defn update-ball-score [state np1-y np2-y]
  (cond (< (state :ball-x) 0) ;; goal p2
        {:ball-x (* 2 (state :p-w)) 
         :ball-y 0
         :ball-vx (state :iball-vx)
         :ball-vy (state :iball-vy)
         :p1-score (state :p1-score)
         :p2-score (+ 1 (state :p2-score))}
         
        (> (state :ball-x) (q/width)) ;; goal p1
        {:ball-x (* 2 (state :p-w))
         :ball-y 0
         :ball-vx (state :iball-vx)
         :ball-vy (state :iball-vy)
         :p1-score (+ 1 (state :p1-score))
         :p2-score (state :p2-score)}
        
        :else
        (let [nball-x (+ (state :ball-vx) (state :ball-x))
              nball-y (+ (state :ball-vy) (state :ball-y))]
          {:ball-x nball-x
           :ball-y nball-y
           :ball-vx (* (collision-x state nball-x nball-y np1-y np2-y)
                       (state :ball-vx))
           :ball-vy (* (collision-y nball-y)
                       (state :ball-vy))
           :p1-score (state :p1-score)
           :p2-score (state :p2-score)})))

(defn inside-field? [y dy]
  (and (>= y 0) (<= (+ y dy) (q/height))))

(defn update-py [state kp-y kp-up kp-down]
  (let [np-y (+ (state kp-y) (* 5 (+ (state kp-up) (state kp-down))))]
    (if (inside-field? np-y (state :p-h))
      np-y
      (state kp-y))))

(defn update-state [state]
  (let [np1-y (update-py state :p1-y :p1-up :p1-down)
        np2-y (update-py state :p2-y :p2-up :p2-down)
        nball (update-ball-score state np1-y np2-y)]
    (assoc state
           :p1-y np1-y
           :p2-y np2-y
           :ball-x (nball :ball-x)
           :ball-y (nball :ball-y)
           :ball-vx (nball :ball-vx)
           :ball-vy (nball :ball-vy)
           :p1-score (nball :p1-score)
           :p2-score (nball :p2-score))))

(defn draw-net []
  (let [w (q/width)
        h (q/height)
        rect-w (/ w 200)
        rect-h (/ h 50)
        rect-x (- (/ w 2) (/ rect-w 2))
        spacing rect-h]
    (q/fill 255)
    (dotimes [n 25]
      (q/rect rect-x (* n (+ spacing rect-h)) rect-w rect-h))))

(defn draw-scores [state]
  (let [w (q/width)
        h (q/height)]
    (q/text-font (q/create-font "SquareFont" 32))
    (q/text (.toString (state :p1-score)) (/ w 4) (/ h 10))
    (q/text (.toString (state :p2-score)) (* 3 (/ w 4)) (/ h 10))))

(defn draw-players [state]
  (q/rect (state :p1-x) (state :p1-y) (state :p-w) (state :p-h))
  (q/rect (state :p2-x) (state :p2-y) (state :p-w) (state :p-h)))

(defn draw-ball [state]
  (q/rect (state :ball-x) (state :ball-y) (state :ball-w) (state :ball-h)))

(defn draw-state [state]
  (q/background 0 0 0)
  (draw-net)
  (draw-scores state)
  (draw-players state)
  (draw-ball state))

(defn key-pressed [state event]
  (case (:key event)
    :w (assoc state :p1-up -1)
    :s (assoc state :p1-down 1)
    :up (assoc state :p2-up -1)
    :down (assoc state :p2-down 1)
    state))

(defn key-released [state]
  (case (q/key-code)
    87 (assoc state :p1-up 0)
    83 (assoc state :p1-down 0)
    38 (assoc state :p2-up 0)
    40 (assoc state :p2-down 0)
    state))

(q/defsketch pong
  :title "You spin my circle right round"
  :size :fullscreen
  :setup setup
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :middleware [m/fun-mode]
  :key-pressed key-pressed
  :key-released key-released)
