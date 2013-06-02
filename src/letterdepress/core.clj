(ns letterdepress.core
  (:use [clojure.core.reducers :only (cat)])
  (:use [clojure.java.io])
  (:gen-class))

; define a board type with:
; - open keys
; - claimed keys
; - their available keys

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
  (let [state (reduce
          (fn [state c]
            (let [points (state :points 0)
                  tps (state :two-pointers {})
                  ops (state :one-pointers {})
                  tp  (tps c 0)
                  op  (ops c 0)]
              (cond
                (> tp 0) (assoc state
                  :two-pointers (assoc tps c (- tp 1))
                  :points       (+ points 2))
                (> op 0) (assoc state
                  :one-pointers (assoc ops c (- op 1))
                  :points       (+ points 1))
                :else state)))
          (assoc {}
            :two-pointers two-pointers
            :one-pointers one-pointers)
          word)]
    (state :points 0)))

(defn -main
  "I don't do a whole lot ... yet."
  ([un-played]
    (-main un-played ""))
  ([un-played in-play]
    (-main un-played in-play ""))
  ([un-played in-play reserved]
    (let [board          (cat in-play (cat un-played reserved))
          board-freq     (frequencies board)
          freqs (fn [word]
            (compare-freqs (frequencies word) board-freq))
          on-board (fn [word]
            (freqs-pos? (freqs word)))
          un-played-freq (frequencies un-played)
          in-play-freq   (frequencies in-play)
          score (fn [word]
            [word (score word un-played-freq in-play-freq)])
          print-score (fn [[word score]]
            (println (format "%d %s" score word)))]
      (with-open [rdr (reader "/usr/share/dict/words")]
        (doseq [word (map #(.toLowerCase %) (line-seq rdr))
               :when (on-board word)]
          (print-score (score word)))))))
