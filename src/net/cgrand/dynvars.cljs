(ns net.cgrand.dynvars
  (:require-macros [net.cgrand.cljs.js.repl.dynvars :refer [bound-fn binding dynvar with-bindings]])
  (:refer-clojure :exclude [-lookup]))

(def ^:private ^:dynamic *overwritten-root-values*
  "Root values of bound dynvars, stored in a mutable stack (array)
   Its value is mutable because it grows and shrinks with the call stack.
   However it needs to be set when context switching."
  #js [])
(def ^:private ^:dynamic *top-frame* "A snapshot of the dynvars overwritten by the last push-frame!" nil)
(def ^:private ^:dynamic *bound-dynvars* "A bitset (array) of bound dynvars (irrespective of frames)" #js [0])
(def ^:private empty-env
  #js [#js [] nil #js [0]])

(def interns #js {})

(def all-dynvars "Interned dynvars" {})

(defn bitset-test [bs i]
  (let [j (unsigned-bit-shift-right i 5)
        mask (bit-shift-left 1 (bit-and 31 i))]
    (and (< j (.-length bs))
      (not (zero? (bit-and mask (aget bs j)))))))

(defn- bitset-set! [bs bs0 i]
  (let [j (unsigned-bit-shift-right i 5)
        mask (bit-shift-left 1 (bit-and 31 i))]
    (if (>= j (.-length bs))
      (let [bs (if (identical? bs bs0) (aclone bs) bs)]
        (while (> j (.-length bs)) (.push bs 0))
        (doto bs (.push mask)))
      (let [w (aget bs j)
            w' (bit-or w mask)]
        (when-not (== w w')
          (doto (if (identical? bs bs0) (aclone bs) bs) (aset j w')))))))

(defprotocol IDynVar
  (-restore [_ v]))

(deftype DynVar [sym f i]
  Object
  (toString [_]
    (str "#<dynvar " sym " >"))
  IDeref
  (-deref [_] (f))
  IDynVar
  (-restore [_ v] (f v)))

(defn intern-dynvar [sym f]
  (let [dv (DynVar. sym f (count all-dynvars))]
    (set! all-dynvars (assoc all-dynvars sym dv))
    dv))

(defn push-frame!
  [dynvars]
  (let [frame (js/Array. (+ 4 (.-length dynvars)))]
    (aset frame 0 dynvars)
    (aset frame 1 *top-frame*)
    (aset frame 2 *bound-dynvars*)
    (aset frame 3 (.-length *overwritten-root-values*))
    (loop [i 0 bs *bound-dynvars*]
      (when (< i (.-length dynvars))
        (let [dv (aget dynvars i)
              v @dv
              bs (if-some [bs (bitset-set! bs *bound-dynvars* (.-i dv))]
                   (do  (.push *overwritten-root-values* dv v) bs)
                   bs)]
          (aset frame (+ 4 i) v)  
          (recur (inc i) bs))))
    (set! *top-frame* frame)))

(defn pop-frame! []
  (when-not *top-frame*
    (throw (js/Error. "Pop without matching push.")))
  (let [frame *top-frame*
        bound-dynvars (aget frame 0)]
    (set! *top-frame* (aget frame 1))
    (set! *bound-dynvars* (aget frame 2))
    (set! (.-length *overwritten-root-values*) (aget frame 3))
    (loop [i 0]
      (when (< i (.-length dynvars))
        (-restore (aget dynvars i) (aget frame (+ 4 i)))
        (recur (inc i))))))

(defn with-bindings* [m f]
  (push-frame! (to-array (keys m)))
  (try
    (reduce-kv (fn [_ dv v] (-restore dv v)) nil m)
    (f)
    (finally
      (pop-frame!))))

(defn get-thread-bindings
  "Get a map with the DynVar/value pairs which is currently in effect."
  []
  (loop [i 0 m (transient {})]
    (if (< i (.-length *overwritten-root-values*))
      (let [dv (aget *overwritten-root-values* i)]
        (recur (+ 2 i) (assoc! m dv @dv)))
      (persistent! m))))

(defn switch-bindings!
  "Captures the whole dynamic binding environment and replace it with the one passed as argument
   (or nil for an empty environment).
   Returns the captured environment so that it can be switched back.
   An environment is persistent and thus can be switched back several times."
  [env]
  (let [env (or env empty-env)
        env' (let [n (/ (.-length *overwritten-root-values*) 2)
                   a (js/Array. (+ 3 n))
                   orv ]
               (aset a 0 orv)
               (aset a 1 *top-frame*)
               (aset a 2 *bound-dynvars*)
               (loop [i 0]
                 (when (< i n)
                   (let [dv (aget orv (* 2 i))
                         v (aget orv (inc (* 2 i)))]
                     (aset a (+ 3 i) @dv)
                     (-restore dv v)) ; use bitsets to avoid some writes?
                   (recur (inc i))))
               a)
        orv (aclone (aget env 0))
        n (.-length orv)]
    (set! *overwritten-root-values* orv)
    (set! *top-frame* (aget env 1))
    (set! *bound-dynvars* (aget env 2))
    (loop [i 0]
      (when (< i n)
        (-restore (aget orv (* 2 i)) (aget env (+ 3 i)))
        (recur (inc i))))
    env'))

(defn current-binding-env
  "Returns the current binding environemnt."
  []
  ; TODO: optimize
  (let [env (switch-bindings! nil)]
    (switch-bindings! env)
    env))

(defn horizon* [f]
  (let [frame *top-frame*
        dynvars *bound-dynvars*
        orv *overwritten-root-values*]
    (set! *top-frame* nil)
    (set! *bound-dynvars* #js [0])
    (set! *overwritten-root-values* #js [])
    (try
      (f)
      (finally
        (set! *top-frame* frame)
        (set! *bound-dynvars* dynvars)
        (set! *overwritten-root-values* orv)))))