(ns chemistry.core
  (:gen-class))

(def +subshell-L+
  {:s 0
   :p 1
   :d 2
   :f 3})

(def +L-subshell+
  {0 's
   1 'p
   2 'd
   3 'f})

(defmacro n+l
  "addition of quantum numbers; energy; n+l rule"
  [n l]
  (+ n l))

(defn magnetic-numbers
  "Range of 'm' quantum numbers from l quantum number"
  [l]
  (range (* -1 l) (inc l)))

(defn electron-count
  "e^- count from m and l"
  [m l]
  (+ 1
     (Math/abs (- m
                  (* -1 l)))))

(defn angular-numbers
  "Range of angular quantum numbers 'l' from n principal number"
  [n]
  (range (min 4 n)))

(defn quantum-number-combinations
  "Combinations of n, l, m from n"
  [n]
  (for [N (range 1 (inc n))
        L (angular-numbers N)
        M (magnetic-numbers L)]
    {:n N :l L :m M}))

(defn configuration-from-n
  "Electron configuration from n"
  [n]
  (map (fn [x] (let [{N :n
                           L :l
                           M :m} x]
                      (str (apply str [N
                                       (get +L-subshell+ L)
                                       "^"
                                       (electron-count M L)]))))
            (quantum-number-combinations n)))

(defn lower-energy?
  "Aufabu principle. Compare two quantum number combinations. Returns true if x is lower, false if y is lower"
  [x y]
  (let [{x-n :n x-l :l} x
        {y-n :n y-l :l} y
        x-sum (+ x-n x-l)
        y-sum (+ y-n y-l)]
    (cond (< x-sum y-sum) true
          (< y-sum x-sum) false
          (< x-n y-n) true
          :else false))) ;; y-n is lower

(defn sort-quantum-number-combinations
  [combinations]
  (sort lower-energy? combinations))

(defn electron-count-assigned
  [e-count]
  (let [how-many-combs (quot e-count 2)
        remainder-e-count (mod e-count 2)
        all-combs (sort-quantum-number-combinations (quantum-number-combinations e-count))
        used-combs (take how-many-combs all-combs)
        final-comb (nth all-combs (+ (dec how-many-combs)
                                     remainder-e-count))
        avail-combs1 (conj (vec used-combs)
                           final-comb)
        highest-n-l (select-keys final-comb [:n :l])
        before-final? (fn [x] (not (and (= (:l x)
                                           (:l highest-n-l))
                                        (= (:n x)
                                           (:n highest-n-l)))))
        including-final? (fn [x] (let [{L :l N :n} x]
                                   (and (<= L (:l highest-n-l))
                                        (<= N (:n highest-n-l)))))
        avail-combs (take-while including-final? all-combs)] ;; TODO quantum-number-combinations lazy inf seq
    (-> (map #(assoc % :e 2)
             (take-while before-final? avail-combs)) ;; all the combs before the final ones
        vec
        (into (let [rest-combs (drop-while before-final?
                                           avail-combs)
                    avail-electrons (- e-count
                                       (* 2
                                          (- (count avail-combs)
                                             (count rest-combs))))]
                (loop [e-left avail-electrons
                       ret-idx 0
                       ret (vec (map #(assoc % :e 0) rest-combs))]
                  (cond (zero? e-left) ret
                        (= ret-idx
                           (count ret)) (recur e-left 0 ret)
                        :else (recur (dec e-left)
                                     (inc ret-idx)
                                     (assoc-in ret
                                               [ret-idx :e]
                                               (inc (get (nth ret ret-idx)
                                                         :e)))))))))))


(defn text-configuration
  "Textualizes a configuration of {:e electrons :n :l :m ...} to e.g. 2p^3"
  [config]
  )

(defn configuration-from-electron-count
  [e-count]
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
