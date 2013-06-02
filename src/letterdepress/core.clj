(ns letterdepress.core
  (:use [clojure.core.reducers :only (cat)])
  (:use [clojure.java.io])
  (:gen-class))

(defn- compare-freqs
  [word-freq board-freq]
  (into {}
    (for [c (keys word-freq)]
      [c (- (board-freq c 0) (word-freq c))])))

(defn freqs-pos?
  [freqs]
  (reduce (fn [pos [ch freq]] (and pos (>= freq 0))) true freqs))

(defn score
  [word two-pointers one-pointers]
  (reduce
    (fn [state c]
      (let [won-points  (state :won-points  0)
            lost-points (state :lost-points 0)
            tps (state :two-pointers {})
            ops (state :one-pointers {})
            tp  (tps c 0)
            op  (ops c 0)]
        (cond
          (> tp 0) (assoc state
            :two-pointers (assoc tps c (- tp 1))
            :won-points   (inc won-points)
            :lost-points  (inc lost-points))
          (> op 0) (assoc state
            :one-pointers (assoc ops c (- op 1))
            :won-points   (inc won-points))
          :else state)))
    (assoc {}
      :word         word
      :two-pointers two-pointers
      :one-pointers one-pointers)
    word))

(defn- getenv
  ([k]
    (getenv k nil))
  ([k fallback]
    (let [found (System/getenv k)]
      (if (nil? found) fallback found))))

(defn -main
  "I don't do a whole lot ... yet."
  ([in-play]
    (-main in-play ""))
  ([in-play un-played]
    (-main in-play un-played ""))
  ([in-play un-played reserved]
    (let [in-play        (.toLowerCase in-play)
          un-played      (.toLowerCase un-played)
          reserved       (.toLowerCase reserved)
          dict-path      (getenv "DICT" "/usr/share/dict/words")
          board-freq     (frequencies (cat in-play (cat un-played reserved)))
          availability   (fn [word] (compare-freqs (frequencies word) board-freq))
          on-board       (fn [word] (freqs-pos? (availability word)))
          un-played-freq (frequencies un-played)
          in-play-freq   (frequencies in-play)
          score          (fn [word] (score word in-play-freq un-played-freq))
          print-score    (fn [s] (println (format "+%d -%d %s"
                            (s :won-points 0) (s :lost-points 0) (s :word))))]
      (with-open [rdr (reader dict-path)]
        (doseq [word (map #(.toLowerCase %) (line-seq rdr))
               :when (on-board word)]
          (print-score (score word)))))))
