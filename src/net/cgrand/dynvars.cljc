(ns net.cgrand.dynvars
  (:require [cljs.analyzer :as ana])
  (:refer-clojure :exclude [binding]))

(defmacro var [name]
  (let [name (ana/resolve-symbol name)]
    `(or (dynvars '~name) (intern-dynvar '~name (fn 
                                                  ([] ~name)
                                                  ([v#] (set! ~name v#)))))))

(defmacro intern
  "Cache the resulting of evaluating e."
  [e]
  (let [k (name (gensym))]
    `(let [v# (goog.object/get interns ~k interns)]
       (if (identical? interns v#)
         (let [v# ~e]
           (goog.object/set interns ~k v#)
           v#)
         v#))))

(defmacro binding
  "Drop-in replacement for cljs.core/binding except that bindings set using this macro can be conveyed.
   See bound-fn, get-bindings, with-bindings."
  [bindings & body]
  (let [names (set (map ana/resolve-symbol (take-nth 2 bindings)))]
    `(let [dynvars# (intern (array ~@(map #(list `var %) names)))]
       (push-frame! dynvars#)
       ~@(map #(list* `set! %) (partition 2 bindings))
       (try
         ~@body
         (finally
           (pop-frame!))))))

;; clojure-like and mostly wrong
(defmacro with-bindings [m & body]
  `(with-bindings* ~m (fn [] ~@body)))

(defmacro bound-fn [& fntail]
  `(let [bindings# (get-thread-bindings)
         f# (fn ~@fntail)]
     (fn [& args#]
       (with-bindings bindings#
         (apply f# args#)))))

; more experimental
(defmacro continuation [& fntail]
  `(let [env# (current-binding-env)
         f# (fn ~@fntail)]
     (fn [& args#]
       (let [back# (switch-bindings! env#)]
         (try
           (apply f# args#)
           (finally
             (switch-bindings! back#)))))))

(defmacro horizon
  "Creates a dynamic horizon.
   Code executing inside the horizon won't capture dynamic bindings set before the horizon."
  [& body]
  `(horizon* (fn [] ~@body)))
