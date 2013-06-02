(ns letterdepress.core
  (:gen-class)
  (:use [clojure.core.reducers :only (cat)])
  (:use [clojure.java.io]))

(defn- availability
  [word board]
  (into {}
    (for [c (keys word)]
      [c (- (board c 0) (word c))])))

(defn- available?
  [freqs]
  (reduce
    (fn [ok [_ avail]]
      (and ok (>= avail 0)))
    true
    freqs))

(defn- update-score
  [letter
   {won       :won,
    lost      :lost,
    in-play   :in-play,
    un-played :un-played,
    nullified :nullified,
    :as state
    :or {won 0 lost 0}}]
  (let [i (in-play   letter 0)
        u (un-played letter 0)
        n (nullified letter 0)]
    (into state (cond
      (> i 0) {:in-play   (assoc in-play letter (dec i))
               :lost      (inc lost)
               :won       (inc won)}
      (> u 0) {:un-played (assoc un-played letter (dec u))
               :won       (inc won)}
      (> n 0) {:nullified (assoc nullified letter (dec n))}
      :else   {:valid     false}))))

(defn score
  [word in-play un-played nullified]
  (loop [current-score {:in-play   in-play
                        :un-played un-played
                        :nullified nullified
                        :valid     true}
         letters word]
    (if (and (not-empty letters) (current-score :valid))
      (recur (update-score (first letters) current-score) (rest letters))
      current-score)))

(defn print-score
  [word
   {won   :won,
    lost  :lost,
    valid :valid,
    :or {won 0 lost 0 valid false}}]
  (if valid
    (println (format "+%d -%d %s" won lost word))))

(def env ^{:private true}
  (into {} (System/getenv)))

(def dict-path
  (env "DICT" "/usr/share/dict/words"))

(defn -main
  "Letterpress got you sad?  This will make you sadder.
  "
  ([un-played]
    (-main un-played ""))
  ([un-played in-play]
    (-main un-played in-play ""))
  ([un-played in-play nullified]
    (let [un-played (frequencies (.toLowerCase un-played))
          in-play   (frequencies (.toLowerCase in-play))
          nullified (frequencies (.toLowerCase nullified))]
      (with-open [dict (reader dict-path)]
        (doseq [word (line-seq dict)]
          (print-score word
            (score (.toLowerCase word) in-play un-played nullified)))))))
