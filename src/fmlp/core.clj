(ns fmlp.core
  (:gen-class)
  (:use [clojure.core.reducers :only (cat)])
  (:use [clojure.java.io]))

(defn check-game-over
  [{open :open, :as score :or {open {}}}]
    (assoc score :game-over?
      (zero? (reduce + 0 (flatten (vals open))))))

(defn- update-score
  [letter
   {points   :points,
    taken    :taken,
    opponent :opponent,
    open     :open,
    occupied :occupied,
    :as state
    :or {points 0 taken 0}}]
    (check-game-over
      (into state
        (let [unc (open     letter 0)
              opp (opponent letter 0)
              occ (occupied letter 0)]
          (cond
            (pos? unc)
              {:open     (assoc open     letter (dec unc))
               :points   (inc points)}
            (pos? opp)
              {:opponent (assoc opponent letter (dec opp))
               :taken    (inc taken)
               :points   (inc points)}
            (pos? occ)
              {:occupied (assoc occupied letter (dec occ))}
            :else
              {:valid    false})))))

(defn score
  [word open opponent occupied]
    (loop [letters  word
           score    {:word     word
                     :open     open
                     :opponent opponent
                     :occupied occupied
                     :valid    true}]
      (if (and (not-empty letters) (score :valid))
        (recur (rest letters) (update-score (first letters) score))
        score)))

(defn select-top
  [n by col]
    (flatten (vals
      (reduce
        (fn [scoreboard t]
          (if-let [v (by t)]
            (if-let [s (scoreboard v)]
              (assoc scoreboard v (conj s t))
              (if (< (count scoreboard) n)
                (assoc scoreboard v [t])
                (let [lowest (key (first scoreboard))]
                  (if (> v lowest)
                    (assoc (dissoc scoreboard lowest) v [t])
                    scoreboard))))
            scoreboard))
        (sorted-map)
        col))))

(defn select-high-scores
  [n scores]
    (select-top n
      (fn [s] (+ (s :points 0) (s :taken 0)))
      scores))

(defn is-valid [{v :valid :or {v false}}] v)

(defn format-score
  [{points     :points,
    taken      :taken,
    word       :word,
    game-over? :game-over?,
    :or {points 0 taken 0 game-over? false}}]
    (format "%s +%02d -%02d %s"(if game-over? "*" " ") points taken word))

(defn- warn
  [config warning]
    (assoc config :warnings (conj (config :warnings) warning)))

(defn- getopt
  [args config opt-parse]
    (loop [args        args
           config      config]
      (if-let [arg (first args)]
        (recur (rest args)
          (if (= (first arg) \-)
            (if (= (first (rest arg)) \-)
              (opt-parse config arg)
              (reduce opt-parse config (rest arg)))
            (assoc config :args (conj (config :args) arg))))
        config)))

(def default-config
  { :high-scores     0
    :only-game-over? false
    :do-help?        false
    :open            {}
    :opponent        {}
    :occupied        {}
    :warnings        []})

(defn- parse-config
  ([args]
    (parse-config args default-config))
  ([args config]
    (defn- opts
      [c flag]
      (case flag
        \H (assoc c :high-scores (inc (c :high-scores 0)))
        \h (assoc c :do-help? true)
        \o (assoc c :only-game-over? true)
        (warn c (format "Unexpected flag: %s" flag))))
    (let [config  (getopt args config opts)]
      (loop [args (config :args [])
             positions [:open :opponent :occupied]
             config config]
        (if (or (empty? args) (empty? positions))
          config
          (recur (rest args) (rest positions)
            (assoc config (first positions)
              (frequencies (.toLowerCase (first args))))))))))

(def board-sz (* 5 5))

(defn -main
"Letterpress got you sad?  This will make you sadder.
  fmlp open-letters [opponent-letters [occupied-letters]]"
  [& args]
    (let [{open            :open,
           opponent        :opponent,
           occupied        :occupied,
           warnings        :warnings,
           do-help?        :do-help?,
           high-scores     :high-scores,
           only-game-over? :only-game-over?}
                    (parse-config args)

          help-msg  (:doc (meta (var -main)))

          letter-count
                    (reduce + 0 (flatten (map vals (filter not-empty [open opponent occupied]))))

          select-scores
                    (cond
                      only-game-over?   #(filter (fn [s] (s :game-over? false)) %)
                      (> high-scores 0) #(select-high-scores high-scores %)
                      :else identity)

          scores        (fn [words]
                          (select-scores
                            (filter is-valid
                              (map #(score (.toLowerCase %) open opponent occupied)
                                words))))]

      (doseq [w warnings]
        (.println *err* w))

      (cond
        (> letter-count board-sz)
          (.println *err* (format "%d too many letters provided" (- letter-count board-sz)))
        (< letter-count board-sz)
          (.println *err* (format "%d too few letters provided" (- board-sz letter-count))))

      (cond
        do-help?
          (println help-msg)
        (= board-sz 0)
          (.println *err* help-msg)
        :else
          (with-open [dict (reader (resource "words"))]
            (doseq [s (map format-score (scores (line-seq dict)))]
              (println s))))))
