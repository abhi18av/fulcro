(ns com.fulcrologic.fulcro.algorithms.normalized-state-helpers
  "Functions that can be used against a normalized Fulcro state database."
  #?(:cljs (:require-macros com.fulcrologic.fulcro.algorithms.normalized-state-helpers))
  (:refer-clojure :exclude [get-in])
  (:require
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    #?(:clj  [com.fulcrologic.fulcro.dom-server :as dom]
       :cljs [com.fulcrologic.fulcro.dom :as dom])
    [edn-query-language.core :as eql]
    [clojure.spec.alpha :as s]
    [ghostwheel.core :refer [>defn =>]]
    [com.fulcrologic.fulcro.algorithms.data-targeting :as targeting]
    [com.fulcrologic.fulcro.algorithms.denormalize :as fdn]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.components :as comp]))



;;;;;;;;;;;;;;;;;;
;; TODO

;; - [ ] Test this namespace works in CLJC
;; - [ ]  Learn how to run the ghostwheel test in the browser
;; - [ ] Try out generative testing


;; REMOVE this
(def normalized-state {:car/id      {1 {:car/id 1, :car/model "T-150"},
                                     2 {:car/id 2, :car/model "Ferrari"},
                                     3 {:car/id 3, :car/model "Mercedez"}},
                       :person/id   {1 {:person/id     1,
                                        :person/name   "Joe",
                                        :person/age    24,
                                        :person/spouse [:person/id 2],
                                        :person/cars   [[:car/id 1] [:car/id 2]]},
                                     2 {:person/id     2,
                                        :person/name   "Dafny",
                                        :person/age    21,
                                        :person/spouse [:person/id 1],
                                        :person/cars   [[:car/id 3]]}},
                       :root/person [[:person/id 1] [:person/id 2]]})


;; REMOVE this
(def denormalized-state-to-one {:root/person {:person/id     1,
                                              :person/name   "Joe",
                                              :person/age    24,
                                              :person/spouse {:person/id   2,
                                                              :person/name "Dafny",
                                                              :person/age  21,
                                                              :person/cars [{:car/id    3,
                                                                             :car/model "Mercedez"}]}
                                              :person/cars   [{:car/id 1, :car/model "T-150"}
                                                              {:car/id 2, :car/model "Ferrari"}]}})

;; REMOVE this
(def denormalized-state-to-many {:root/person [{:person/id     1,
                                                :person/name   "Joe",
                                                :person/age    24,
                                                :person/spouse {:person/id   2,
                                                                :person/name "Dafny",
                                                                :person/age  21,
                                                                :person/cars [{:car/id    3,
                                                                               :car/model "Mercedez"}]}
                                                :person/cars   [{:car/id 1, :car/model "T-150"}
                                                                {:car/id 2, :car/model "Ferrari"}]}
                                               {:person/id   3,
                                                :person/name "Billy",
                                                :person/age  26,
                                                :person/cars [{:car/id 4, :car/model "Tesla"}]}]})





;;============================================================================

;; helper
(defn create-new-path [state path]
  (loop [[h & t] path
         new-path []]
    (if h
      (let [np (conj new-path h)
            c (clojure.core/get-in state np)]
        (if (eql/ident? c)
          (recur t c)
          (recur t (conj new-path h))))
      new-path)))

(>defn tree-path->db-path
  "Convert a 'denormalized' path into a normalized one by walking the path in state and honoring ident-based edges.

  For example, one might find this to be true for a normalized db:

  ```
  state => {:person/id {1 {:person/id 1 :person/spouse [:person/id 3]}
                        3 {:person/id 3 :person/first-name ...}}}

  (tree-path->db-path state [:person/id 1 :person/spouse :person/first-name])
  => [:person/id 3 :person/first-name]
  ```
  "
  ([state path]
   [map? vector? => vector?]
   (let [new-path (create-new-path state path)]
     (if (not= path new-path)
       new-path
       path)))

  ([state path not-found]
   [map? vector? any? => vector?]
   (let [new-path (create-new-path state path)]
     (if (not= path new-path)
       new-path
       not-found))))



(comment

  '())


;;============================================================================

(>defn get-in
  "Just like clojure.core/get-in, but if an element of the path is an ident it will follow the ident instead."
  ([state-map path]
   [map? vector? => any?]
   (clojure.core/get-in state-map (tree-path->db-path state-map path)))

  ([state-map path not-found]
   [map? vector? any? => any?]
   (clojure.core/get-in state-map (tree-path->db-path state-map path) not-found)))

(comment

  '())



;;============================================================================


;; TODO: see deep-remove-entity* (from incubator). I think the incubator implementation is technically insufficient. GC
;; TODO should check/clean all tables, but only one level deep (not a walk). Also, the cascading would be useful to implement.
(>defn remove-entity*
  "Remove the given entity at the given ident. Also scans all tables and removes any to-one or to-many idents that are
   found that match `ident` (removes dangling pointers to the removed entity).

   The optional `cascade` parameter is a set of keywords that represent edges that should cause recursive deletes
   (i.e. it indicates edge names that *own* something, indicating it is safe to remove those entities as well).

   Returns the new state map with the entity(ies) removed."

  ;;TODO
  ([state-map ident]
   [map? eql/ident? => map?]
   )

  ;;TODO
  ([state-map ident cascade]
   [map? eql/ident? (s/coll-of keyword? :kind set?) => map?]
   )
  )



(defn- dissoc-in
  "Remove the given leaf of the `path` from recursive data structure `m`"
  [m path]
  (cond-> m
          (clojure.core/get-in m (butlast path))
          (update-in (butlast path) dissoc)))

(defn- extract-all-idents
  "Extracts all idents present in a children of a given entity"
  [items]
  (into []
        (comp (keep (fn [v]
                      (cond
                        (eql/ident? v)
                        [v]

                        (and (vector? v)
                             (every? eql/ident? v))
                        v)))
              cat)
        (vals items)))


(defn- dissoc-ident-from-top-level [state-map ident]
  (cond-> state-map
          (clojure.core/get-in state-map (butlast ident))
          (update-in (butlast ident) dissoc (second ident))))

(comment
  (dissoc-ident-from-top-level normalized-state [:person/id 1])

  '())


;; TODO remove the dangling pointers to an ident
(defn- dissoc-ident-from-nested-tables [state-map ident]
  (cond-> state-map
          (clojure.core/get-in state-map (butlast ident))
          (update-in (butlast ident) dissoc (second ident))))

(comment
  (dissoc-ident-from-nested-tables normalized-state [:person/id 1])

  '())




;; REMOVE
(defn deep-remove-entity*
  "Recursively remove a table entry (by ident) and anything it recursively points to."
  [state-map ident]
  (let [item (get-in state-map ident)
        idents (extract-all-idents item)]
    (reduce
      ;; reducing function
      (fn [s i] (deep-remove-entity* s i))
      ;; initial state of accumulator
      (dissoc-in state-map ident)
      ;; a collection of values
      idents)))



(comment


  (deep-remove-entity* normalized-state [:person/id 1])

  (extract-all-idents (get-in normalized-state [:person/id 1]))

  (let [item (get-in state-map ident)
        idents (extract-all-idents item)]

    (reduce
      (fn [s i] (deep-remove-entity* s i))
      (dissoc-in state-map ident)
      idents))

  '())



;;============================================================================

;; TODO
;; - [ ] `sort-identy-by`
;;    (sort-idents-by :entity/field vector-of-idents)
;;    This can enable
;;    (swap! state update-in [:entity 1 :list] sort-idents-by :list/field)



;;============================================================================

;; NOTE This is pretty much remove-ident*, but with the option of recursively removing the stuff
;; NOTE assuming db-path
(defn remove-edge*
  ;; TODO
  ([state-map path-to-edge])

  ;; TODO needs cascade argument like remove-entity*
  ([state-map path-to-edge cascade])
  )

(comment

  (defn remove-ident*
    "Removes an ident, if it exists, from a list of idents in app state. This
    function is safe to use within mutations."
    [state-map ident path-to-idents]
    {:pre [(map? state-map)]}
    (let [new-list (fn [old-list]
                     (vec (filter #(not= ident %) old-list)))]
      (update-in state-map path-to-idents new-list)))

  )



;============================================================================
;; REMOVE
(defsc A [this props]
  {:ident         :person/id
   :query         [:person/id :person/name]
   :initial-state {:person/id 1 :person/name "Alice"}
   :extra-data    42}
  (dom/div "TODO"))


(def ui-a (comp/factory A))


(defn ui->props
  "Obtain a tree of props for a UI instance from the current application state. Useful in mutations where you want
  to denormalize an entity from the state database. `this` can often be obtained from the mutation `env` at the
  `:component` key."
  ([this]
   (ui->props (comp/component->state-map this) (comp/react-type this) (comp/get-ident this)))
  ([state-map component-class ident]
   (fdn/db->tree (comp/get-query component-class state-map) (get-in state-map ident) state-map)))


(comment

  (comp/get-ident ui-a)

  (comp/component->state-map ui-a)

  (comp/component->state-map A)

  (comp/react-type A)

  (comp/get-ident A normalized-state)
  (comp/get-query A)


  '())

;============================================================================

;;NOTE These might belong in mutation ns???
(defn update-caller!
  "Runs clojure.core/update on the table entry in the state database that corresponds to the mutation caller (which
   can be explicitly set via `:ref` when calling `transact!`).

   Equivalent to `(swap! (:state env) update-in (:ref env) ...)`."
  [{:keys [state ref] :as mutation-env} & args]
  (apply swap! state update-in ref args))


(comment

  (swap! (:state env) update-in (:ref env))

  '())

;============================================================================

;;NOTE These might belong in mutation ns???
(defn update-caller-in!
  "Like swap! but starts at the ref from `env`, adds in supplied `path` elements (resolving across idents if necessary).
   Finally runs an update-in on that resultant path with the given `args`.
   Roughly equivalent to:

   ```
   (swap! (:state env) update-in (tree-path->db-path @state (into (:ref env) path)) args)
   ```

   with a small bit of additional sanity checking."
  [{:keys [state ref] :as mutation-env} path & args]
  (let [path (tree-path->db-path @state (into ref path))]
    (if (and path (get-in @state path))
      (apply swap! state update-in path args)
      @state)))


;============================================================================

;; TODO: Untested...make up an env with a state atom and see if it works in clj/cljs

#?(:clj
   (defmacro swap*                                          ;; or alternate name could be `state-swap!`
     "A macro that is equivalent to:

     ```
     (swap! (:state env) (fn [s] (-> s ...forms...)))
     ```

     E.g.

     ```
     (swap!-> env
       (merge/merge-component ...)
       (integrate-ident* ...))
     ```


     "
     [mutation-env & forms]
     `(swap! (:state ~mutation-env) (fn [s#]
                                      (-> s#
                                          ~@forms)))))




;;================== ALIASES ============================

;; NOTE: These are already tested in their respective test suites
;; aliases to integrate-ident* and remove-ident*, with doc strings...

(def integrate-ident*
  "[state ident & named-parameters]

  Integrate an ident into any number of places in the app state. This function is safe to use within mutation
  implementations as a general helper function.

  The named parameters can be specified any number of times. They are:

  - append:  A vector (path) to a list in your app state where this new object's ident should be appended. Will not append
  the ident if that ident is already in the list.
  - prepend: A vector (path) to a list in your app state where this new object's ident should be prepended. Will not place
  the ident if that ident is already in the list.
  - replace: A vector (path) to a specific location in app-state where this object's ident should be placed. Can target a to-one or to-many.
   If the target is a vector element then that element must already exist in the vector.

  NOTE: `ident` does not have to be an ident if you want to place denormalized data.  It can really be anything.

  Returns the updated state map."
  targeting/integrate-ident*)

(def remove-ident*
  " [state-map ident path-to-idents]

  Removes an ident, if it exists, from a list of idents in app state. This
  function is safe to use within mutations."
  merge/remove-ident*)

