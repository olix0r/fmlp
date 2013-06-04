:13(ns fmlp.core
  (:gen-class)
  (:use [clojure.core.reducers :only (cat)])
  (:use [clojure.set])
  (:use [clojure.java.io]))

(defn- update-score
  [letter
   {points   :points,
    taken    :taken,
    opponent :opponent,
    open     :open,
    occupied :occupied,
    :as state
    :or {points 0 taken 0}}]
    (let [unc (open     letter 0)
          opp (opponent letter 0)
          occ (occupied letter 0)]
      (into state (cond
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
          {:valid    false}))))

(defn check-game-over
  [score]
    (assoc score :game-over (zero? (reduce + 0 (flatten (vals (score :open)))))))

(defn check-game-overs
  [scores]
    (map check-game-over scores))

(defn score
  [word
   {open     :open,
    opponent :opponent,
    occupied :occupied,
    :or {open {}
         opponent {}
         occupied {}}}]
    (loop [letters  word
           score    {:word     word
                     :open     open
                     :opponent opponent
                     :occupied occupied
                     :valid    true}]
      (if (and (not-empty letters) (score :valid))
        (recur (rest letters) (update-score (first letters) score))
        score)))

(defn select-high-scores
  [n scores]
    (flatten (vals (reduce
      (fn [high-scores
           {points :points :as score :or {points 0}}]
        (if-let [s (high-scores points)]
          (assoc high-scores points (conj s score))
          (if (< (count high-scores) n)
            (assoc high-scores points [score])
            (let [lowest (key (first high-scores))]
              (if (> points lowest)
                (assoc (dissoc high-scores lowest) points [score])
                high-scores)))))
      (sorted-map)
      scores))))

(defn is-valid
  [score]
    (score :valid false))

(defn format-score
  [{points  :points,
    taken   :taken,
    word    :word,
    game-over? :game-over,
    :or {points 0 taken 0} :as score}]
    (format "%s +%02d -%02d %s"(if game-over? "*" " ") points taken word))

(defn format-scores
  [scores]
    (map format-score scores))

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

(defn- parse-config
  ([args] (parse-config args {}))
  ([args config]
    (defn- opts
      [c flag]
      (case flag
        \H (assoc c :high-scores (inc (c :high-scores 0)))
        \h (assoc c :do-help? true)
        (warn c (format "Unexpected flag: %s" flag))))
    (let [config  (getopt args config opts)]
      (loop [args (config :args [])
             positions [:open :opponent :occupied]
             config config]
        (if (or (empty? args) (empty? positions))
          config
          (recur (rest args) (rest positions) (assoc config
            (first positions) (frequencies (.toLowerCase (first args))))))))))

(def board-capacity (* 5 5))

(defn -main
"Letterpress got you sad?  This will make you sadder.
  fmlp open-letters [opponent-letters [occupied-letters]]"
  [& args]
    (let [{open        :open,
           opponent    :opponent,
           occupied    :occupied,
           warnings    :warnings,
           do-help?    :do-help?,
           high-scores :high-scores,
           :as config
           :or {open     {}
                opponent {}
                occupied {}
                warnings []
                do-help? false
                high-scores 0}}
                    (parse-config args {})

          help-msg  (:doc (meta (var -main)))

          board-sz  (reduce + 0 (flatten (map vals (filter not-empty [open opponent occupied]))))

          select-scores
                    (if (> high-scores 0)
                      #(select-high-scores high-scores %)
                      identity)

          scores    (fn [words]
                      (map check-game-over
                        (select-scores
                          (filter is-valid
                            (map #(score (.toLowerCase %) config)
                              words)))))]

      (doseq [w warnings]
        (.println *err* w))

      (if (> board-sz board-capacity)
        (.println *err* (format "%d too many letters provided" (- board-sz board-capacity)))
        (if (< board-sz board-capacity)
          (.println *err* (format "%d too few letters provided" (- board-capacity board-sz)))))

      (cond
        do-help?
          (println help-msg)
        (= board-sz 0)
          (.println *err* help-msg)
        :else
          (with-open [dict (reader (resource "words"))]
            (doseq [s (format-scores (scores (line-seq dict)))]
              (println s))))))
