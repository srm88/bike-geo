(ns bike-geo)

;; Generic utils

(defn right-pad
  [size text]
  (str text (apply str (repeat (- size (count text)) " "))))

;; https://stackoverflow.com/a/25098576
(defn round-to
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

;; Wheel helpers

(defn wheel-radius
  [bsd tire-width]
  (+ (/ bsd 2) tire-width))

(def bsd-700c 622)
(def bsd-650b 584)

(def radius-700c (partial wheel-radius bsd-700c))
(def radius-650b (partial wheel-radius bsd-650b))

;; Big functions

(defn front-center
  [{bb-drop :bb-drop stack :stack reach :reach fork-offset :fork-offset hta :hta}]
  (let [hta* (Math/toRadians hta)
        ht-projection-y (- stack bb-drop)
        ht-projection-x (/ ht-projection-y (Math/tan hta*))
        fork-offset-x (/ fork-offset (Math/sin hta*))
        bb-to-axle-x (+ reach ht-projection-x fork-offset-x)
        bb-angle (Math/atan2 bb-to-axle-x bb-drop)]
    (/ bb-to-axle-x (Math/sin bb-angle))))

(defn rear-center
  [{bb-drop :bb-drop chainstay :chainstay}]
  (->> (/ bb-drop chainstay) ; vertical (adjacent) component over hypotenuse
       (Math/acos)           ; arccos to derive angle
       (Math/sin)            ; take sine
       (* chainstay)))       ; multiply by hypotenuse to yield horizontal component

(defn wheelbase
  [bike]
  (+ (rear-center bike) (front-center bike)))

(defn toe-room*
  [bsd crank-len bike]
  (- (front-center bike)
     (wheel-radius bsd (:tire bike))
     crank-len))

;; 175mm cranks, 700c are most common.
(def toe-room (partial toe-room* bsd-700c 175))

;; Bike definitions and functions

(defn tire-setter [width] (fn [bike] (assoc bike :tire width)))

(def riddlers (tire-setter 45))
(def nanos (tire-setter 40))
(def rock-n-roads (tire-setter 43))

(def black-mtn-60 {:name "black mtn 60"
                   :bb-drop 70
                   :stack 623
                   :reach 394
                   :fork-offset 50
                   :hta 72
                   :chainstay 432})

(def vaya-595 {:name "vaya 59.5"
               :bb-drop 75
               :stack 647
               :reach 384.8
               :fork-offset 50
               :hta 71.5
               :chainstay 450})

(def rove-56 {:name "rove 56"
              :bb-drop 65
              :stack 585
              :reach 383
              :fork-offset 43
              :hta 71.5
              :chainstay 435})

(def rove-59 (assoc rove-56
                    :name "rove 59"
                    :stack 615
                    :reach 396
                    :hta 72))

;; This is of course the masi CXGR
(def masi-56 {:name "masi 56"
              :bb-drop 65
              :stack 577
              :reach 379
              :fork-offset 45
              :hta 72
              :chainstay 432})

(def masi-58 (assoc masi-56
                    :name "masi 58"
                    :stack 590
                    :reach 393))

(def scott (riddlers black-mtn-60))
(def jon (nanos masi-56))
(def zack (rock-n-roads rove-56))

(def tires [riddlers
            rock-n-roads
            nanos])

(def bikes [black-mtn-60
            vaya-595
            rove-56
            rove-59
            masi-56
            masi-58])

(defn toe-room-report
  [tires bikes]
  (let [cols (+ 3 (apply max (map (comp count :name) bikes)))]
    (->> (for [tire-fn tires]
           ["---\n"
            (for [bike bikes]
              [(right-pad cols (str " " (:name bike)))
               (round-to 1 (toe-room (tire-fn bike)))
               "\n"])])
         (flatten)
         (apply str))))

(defn- simple-report
  [value-fn]
  (fn [bikes]
    (let [cols (+ 2 (apply max (map (comp count :name) bikes)))]
      (->> (for [bike bikes]
             [(right-pad cols (:name bike))
              (round-to 1 (value-fn bike))
              "\n"])
           (flatten)
           (apply str)))))

(def wheelbase-report (simple-report wheelbase))
(def front-center-report (simple-report front-center))
(def rear-center-report (simple-report rear-center))

(defn big-report
  []
  (->> [bikes "\n"
        "= Wheelbase\n" (wheelbase-report bikes)
        "\n= Rear center\n" (rear-center-report bikes)
        "\n= Front center\n" (front-center-report bikes)
        "\n= Toe room\n" (toe-room-report tires bikes)]
       (flatten)
       (apply str)
       (print)))
