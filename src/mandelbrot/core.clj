(ns mandelbrot.core
  (:import java.awt.image.BufferedImage
           (java.awt Color RenderingHints))
  )

(defn- escape
  "Returns an integer indicating how many iterations were required before the
  value of z (using the components `a` and `b`) could be determined to have
  escaped the Mandelbrot set. If z will not escape, -1 is returned."
  [^double a0 ^double b0 depth]
  (loop [a a0
         b b0
         iteration 0]
    (cond
      (< 4 (+ (* a a) (* b b))) iteration
      (>= iteration depth) -1
      :else (recur (+ a0 (- (* a a) (* b b)))
                   (+ b0 (* 2 (* a b)))
                   (inc iteration)))))

(defn mandelbrot
  "Calculates membership within and number of iterations to escape from the
  Mandelbrot set for the region defined by `rmin`, `rmax` `imin` and `imax`
  (real and imaginary components of z, respectively).  Optional kwargs include
  `:depth` (maximum number of iterations to calculate escape of a point from
  the set), `:height` ('pixel' height of the rendering), and `:width`
  ('pixel' width of the rendering).  Returns a seq of row vectors containing
  iteration numbers for when the corresponding point escaped from the set. -1
  indicates points that did not escape in fewer than `depth` iterations, i.e.
  they belong to the set. These integers can be used to drive most common
  Mandelbrot set visualizations."
  [rmin rmax imin imax & {:keys [width height depth]
                          :or {width 80 height 40 depth 1000}}]
  (let [rmin (double rmin)
        imin (double imin)
        stride-w (/ (- rmax rmin) width)
        stride-h (/ (- imax imin) height)]
    (loop [x 0
           y (dec height)
           escapes []]
      (if (== x width)
        (if (zero? y)
          (partition width escapes)
          (recur 0 (dec y) escapes))
        (recur (inc x) y (conj escapes (escape (+ rmin (* x stride-w))
                                               (+ imin (* y stride-h))
                                               depth)))))))

(defn render-text
  [mandelbrot-grid]
  (doseq [row mandelbrot-grid]
    (doseq [escape-iter row]
      (print (if (neg? escape-iter) \* \space)))
    (println)))

(defn render-image
  "Does what it says bro, does what it says"
  [mandelbrot-grid]
  (let [palette (vec (for [c (range 500)]
                       (Color/getHSBColor (/ (Math/log c) (Math/log 500)) (/ (Math/log 500) (Math/log c)) 0.5)))
        height (count mandelbrot-grid)
        width (count (first mandelbrot-grid))
        img (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        ^java.awt.Graphics2D g (.getGraphics img)]
    (doseq [[y row] (map-indexed vector mandelbrot-grid)
            [x escape-iter] (map-indexed vector row)]
      (.setColor g (if (neg? escape-iter)
                     (palette 0)
                     (palette (mod (dec (count palette)) (inc escape-iter)))))
      (.drawRect g x y 1 1))
    (.dispose g)
    img))





(defn foo
  "I don't do a whole lot."
  []
  (println "no args or errors")
  (javax.imageio.ImageIO/write
    (render-image
      (mandelbrot -2.25 0.75 -1.5 1.5 :width 3200 :height 2400 :depth 1000))
    "png"
    (java.io.File. "mandelbrot.png"))
  )
