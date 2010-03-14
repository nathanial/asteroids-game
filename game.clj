(ns game
  (:import (java.awt Shape Rectangle Polygon Graphics2D Color Dimension)
	   (java.awt.image BufferedImage)
	   (java.awt.event KeyListener KeyAdapter KeyEvent)
	   (java.util Random)
	   (java.awt.geom AffineTransform )
	   (javax.swing JFrame)))

(def frame (ref nil))
(def gameloop (agent nil))
(def running (ref false))
(def backbuffer (ref nil))
(def g2d (ref nil))
(def asteroids (ref nil))
(def bullets (ref nil))
(def current-bullet (ref 0))
(def ship (ref nil))
(def height 600) 
(def width 600)

(defn bounded [_min _max val]
  (cond
    (> _min val) (- _max (- _min val))
    (< _max val) (- _min (- _max val))
    :else val
    ))

(defn bounded-cirlce [val] (bounded 0 360 val))

(defstruct base-vector-shape 
  :shape :alive :x :y
  :velX :velY :moveAngle :faceAngle)

(defn base-vector-shape-create []
  (struct-map base-vector-shape
    :alive false
    :x 0.0
    :y 0.0
    :velX 0.0
    :velY 0.0
    :moveAngle 0.0
    :faceAngle 0.0))

(defn ship-create []
  (let [bvs (base-vector-shape-create)
	shipx [-6 -3  0  3  6  0]
	shipy [ 6  7  7  7  6 -7]]
    (assoc bvs 
	   :alive true
	   :shape (Polygon. (int-array shipx) 
			    (int-array shipy)
			    (count shipx)))))

(defn ship-bounds [ship]
  (Rectangle. (- (:x ship) 6) (- (:y ship) 6) 12 12))

(defn bullet-create []
  (let [bvs (base-vector-shape-create)]
    (assoc bvs
      :alive false
      :shape (Rectangle. 0 0 1 1))))

(defn bullet-bounds [bullet]
  (Rectangle. (:x bullet) (:y bullet) 1 1))

(defn asteroid-create []
  (let [bvs (base-vector-shape-create)
	astx [-20 -13 0 20 22 20 12 2 -10 -22 -16]
	asty [20 23 17 20 16 -20 -22 -14 -17 -20 -5]]
    (assoc bvs
      :shape (Polygon. (int-array astx) (int-array asty) (count astx))
      :alive true
      :rotVel 0.0)))

(defn asteroid-bounds [asteroid]
  (Rectangle. (- (:x asteroid) 20) (- (:y asteroid) 20) 40 40))

(defn calc-angle-move-x [angle]
  (Math/cos (* angle (/ Math/PI 180))))

(defn calc-angle-move-y [angle]
  (Math/sin (* angle (/ Math/PI 180))))

(def swing-agent (agent nil))

(defmacro swing [& body]
  `(send swing-agent
	 (fn [x#]
	   (javax.swing.SwingUtilities/invokeLater
	    (fn []
	      ~@body
	      )))))

(defn paint []
  (let [frame @frame
	graphics (.getGraphics frame) 
	bb @backbuffer]
    (.drawImage graphics @backbuffer 0 0 frame)))

(defn draw-ship []
  (let [g2d @g2d
	ship @ship]
    (doto g2d
      (.setTransform (AffineTransform.))
      (.translate (:x ship) (:y ship))
      (.rotate (Math/toRadians (:faceAngle ship)))
      (.setColor Color/ORANGE)
      (.fill (:shape ship)))))

(defn draw-bullets []
  (let [bullets @bullets
	g2d @g2d]
    (doseq [bullet (filter :alive bullets)]
      (doto g2d
	(.setTransform (AffineTransform.))
	(.translate (:x bullet) (:y bullet))
	(.setColor Color/MAGENTA)
	(.draw (:shape bullet))))))

(defn draw-asteroids []
  (let [asteroids  @asteroids
	g2d @g2d]
    (doseq [asteroid (filter :alive asteroids)]
      (doto g2d
	(.setTransform (AffineTransform.))
	(.translate (:x asteroid) (:y asteroid))
	(.rotate (Math/toRadians (:moveAngle asteroid)))
	(.setColor Color/DARK_GRAY)
	(.fill (:shape asteroid))))))

(defn update []
  (let [g2d @g2d
	frame @frame
	ship @ship]
    (swing
      (doto g2d
	(.setTransform (AffineTransform.))
	(.setPaint Color/WHITE)
	(.fillRect 0 0 width height)
	(.setColor Color/BLACK)
	(.drawString (str "Ship: " (Math/round (:x ship)) ", " (Math/round (:y ship))) 5 35)
	(.drawString (str "Move angle: " (+ 90 (:moveAngle ship))) 5 50)
	(.drawString (str "Face angle: " (:faceAngle ship)) 5 65))
      (draw-ship)
;      (draw-bullets)
      (draw-asteroids)
      (paint))))
					


(defn update-ship []
  (dosync 
   (ref-set ship
	    (let [ship @ship
		  {:keys [x y velX velY]} ship
		  x (+ velX x)
		  y (+ velY y)]
	      (assoc ship
		:x (bounded -10 (+ 10.0 width) x)
		:y (bounded -10 (+ 10.0 height) y))))))

(defn update-bullets []
  (dosync 
   (ref-set bullets
	    (let [bullets @bullets]
	      (for [bullet bullets]
		(if (not (:alive bullet))
		  bullet
		  (let [{:keys [x y velX velY]} bullet]
		    (assoc bullet
		      :x (+ x velX)
		      :y (+ y velY)
		      :alive (not (or (> 0 (+ y velY))
				      (> 0 (+ x velX))
				      (< height (+ y velY))
				      (< width (+ x velX))))
		      ))))))))

(defn update-asteroids []
  (dosync
   (ref-set asteroids
	    (let [asteroids @asteroids]
	      (for [ast asteroids]
		(if (not (:alive ast))
		  ast
		  (let [{:keys [x y velX velY moveAngle rotVel]} ast
			x (+ x velX)
			y (+ y velY)
			rotVel 1
			moveAngle (+ moveAngle rotVel)]
		    (assoc ast
		      :x (bounded -20 (+ width 20) x)
		      :y (bounded -20 (+ height 20) y)
		      :moveAngle (bounded 0 360 moveAngle))))))))
  nil)

(defn check-collisions [])

(defn start []
  (dosync 
   (ref-set running true)
   (doto (Thread. 
	  (fn []
	    (loop []
	      (update-ship)
	      (update-bullets)
	      (update-asteroids)
	      (check-collisions)
	      (update)
	      (Thread/sleep 20)
	      (when @running
		(recur)))))
     (.start))))

(defn stop []
  (dosync
   (ref-set running false)))

(defn ship-set-velocity [#^Double x #^Double y]
  (dosync 
   (alter ship assoc :velX x :velY y)))

(defn close []
  (swing
    (.setVisible @frame false)))

(defn handle-key-event [k]
  (let [code (.getKeyCode k)]
    (cond 
      (= KeyEvent/VK_LEFT code) 
      (dosync 
       (alter ship assoc :faceAngle (bounded-cirlce (- (:faceAngle @ship) 10))))
      
      (= KeyEvent/VK_RIGHT code)
      (dosync
       (alter ship assoc :faceAngle (bounded-cirlce (+ 10 (:faceAngle @ship)))))
      
      (= KeyEvent/VK_UP code)
      (dosync
       (let [moveAngle (- (:faceAngle @ship) 90)]
	 (alter ship assoc 
		:moveAngle moveAngle
		:velX (* 0.8 (calc-angle-move-x moveAngle))
		:velY (* 0.8 (calc-angle-move-y moveAngle))))))))

(defn create-key-listener []
  (proxy [KeyAdapter] []
    (keyPressed [k] (handle-key-event k))))

(defn init []
  (let [rand (Random.)
	bb (BufferedImage. 600 600 BufferedImage/TYPE_INT_RGB)]
    (dosync 
     (ref-set frame
	      (doto (JFrame.)
		(.setSize (Dimension. 600 600))
		(.setResizable false) 
		(.setVisible true)))
     (ref-set backbuffer bb)
     (ref-set ship (assoc (ship-create) :x 300 :y 300))
     (ref-set g2d (.createGraphics bb))
     (ref-set bullets (for [i (range 10)] (bullet-create)))
     (ref-set asteroids (for [i (range 20)]
			  (let [move-angle (.nextInt rand 360)]
			    (assoc (asteroid-create)
			      :rotVel (+ 1 (.nextInt rand 3))
			      :x (+ 20 (.nextInt rand 600))
			      :y (+ 20 (.nextInt rand 440))
			      :moveAngle move-angle
			      :velX (calc-angle-move-x (- move-angle 90))
			      :velY (calc-angle-move-y (- move-angle 90))
			      ))))))
  (.addKeyListener @frame (create-key-listener)))