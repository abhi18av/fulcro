(ns com.fulcrologic.fulcro.algorithms.normalized-state-helpers
  "Functions that can be used against a normalized Fulcro state database."
  #?(:cljs (:require-macros com.fulcrologic.fulcro.algorithms.normalized-state-helpers))
  (:refer-clojure :exclude [get-in])
  (:require
    [clojure.walk :as walk]
    [clojure.set :as set]
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


;;============================================================================

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
   (loop [[h & t] path
          new-path []]
     (if h
       (let [np (conj new-path h)
             c  (clojure.core/get-in state np)]
         (if (eql/ident? c)
           (recur t c)
           (recur t (conj new-path h))))
       (if (not= path new-path)
         new-path
         path)))))

(comment

  '())


;;============================================================================

(>defn get-in
  "Just like clojure.core/get-in, but if an element of the path is an ident it will follow the ident instead."
  ([state-map path]
   [map? vector? => any?]
   (get-in state-map path nil))

  ([state-map path not-found]
   [map? vector? any? => any?]
   (clojure.core/get-in state-map (tree-path->db-path state-map path) not-found)))


(comment

  '())



;;============================================================================

;; helper
(defn- paths
  "Walks the tree in a depth first manner and returns the possible paths"
  [m]
  (letfn [(paths* [ps ks m]
            (reduce-kv
              (fn [ps k v]
                (if (map? v)
                  (paths* ps (conj ks k) v)
                  (conj ps (conj ks k))))
              ps
              m))]
    (paths* () [] m)))



(defn- dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
             nested structure. keys is a sequence of keys. Any empty maps that result
             will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(defn- nil-or-vector?
  "Predicate to check wheter the argument is a `nil` of a single `vector` "
  [a-value]
  (or (nil? a-value)
    (vector? a-value)))

(defn- vector-of-vectors?
  "Predicate to check whether the argument is a strictly vector of vectors"
  [a-value]
  (if (and
        (vector? a-value)
        (every? vector? a-value))
    true
    false))

;; MAYBE could make this function accept multiple idents
(defn- state-after-top-level-ident-dissoc
  "Returns the state map after top-level `dissoc` of the entity"
  [state-map ident]
  (dissoc-in state-map ident))

(defn- all-paths-after-top-level-dissoc
  "Returns a sequence of all possible path vectors in the state-map"
  [state-map ident]
  (paths (state-after-top-level-ident-dissoc state-map ident)))


(defn- all-values-at-path-after-top-level-dissoc
  "Returns a sequence of all values corresponding to the path vectors in a state-map.
  Contains the `nil` introduced for an `ident` by the `state-after-top-level-ident-dissoc`"
  [state-map ident]
  (let [value-at-path (fn [a-path]
                        (if (>= (count a-path) 4)
                          ;; don't follow idents for denormalized paths
                          (clojure.core/get-in (state-after-top-level-ident-dissoc state-map ident)
                            a-path)
                          ;; follow idents for denormalized paths
                          (get-in (state-after-top-level-ident-dissoc state-map ident)
                            a-path)))]
    (map (fn [a-path]
           (if (map? (value-at-path a-path))
             ;; finds db-path from the original app-db
             (tree-path->db-path state-map a-path)
             (value-at-path a-path)))
      (all-paths-after-top-level-dissoc state-map ident))))


(defn- entity-path-value-map-after-top-level-dissoc
  "Returns a map of all path vectors and their corresponding values.
  Contains the paths and their corresponding values (both normalized and denormalized).
  The values contain the `nil` introduced for an `ident` by the `state-after-top-level-ident-dissoc`"
  [state-map ident]
  (zipmap (all-paths-after-top-level-dissoc state-map ident)
    (all-values-at-path-after-top-level-dissoc state-map ident)))


(defn- prune-ident
  "This is the reducing function used to prune the `nil` values which are the dangling pointers to
  a an entity which is already removed."
  [state-map [a-path a-value] ident]
  ;; if denormalized path, do nothing
  (if (>= (count a-path) 4)
    state-map
    (cond
      (nil? a-value) (dissoc-in state-map a-path)

      (vector-of-vectors? a-value) (assoc-in state-map a-path
                                     (apply vector (remove #{ident} a-value)))
      :else state-map)))

;;====

(>defn remove-entity*
  "Remove the given entity at the given ident. Also scans all tables and removes any to-one or to-many idents that are
   found that match `ident` (removes dangling pointers to the removed entity).

   The optional `cascade` parameter is a set of keywords that represent edges that should cause recursive deletes
   (i.e. it indicates edge names that *own* something, indicating it is safe to remove those entities as well).

   Returns the new state map with the entity(ies) removed."

  ([state-map ident]
   [map? eql/ident? => map?]
   (remove-entity* state-map ident #{}))

  ;;TODO implement the cascading feature

  ([state-map ident cascade]
   [map? eql/ident? (s/coll-of keyword? :kind set?) => map?]

   (reduce #(prune-ident %1 %2 ident)
     (state-after-top-level-ident-dissoc state-map ident)
     (entity-path-value-map-after-top-level-dissoc state-map ident))))

(declare remove-entity-tk*)

(defn- cascade-delete*
  [state-map starting-entity cascade]
  (reduce
    (fn [s edge]
      (if (every? eql/ident? edge)
        (reduce (fn [s2 ident] (remove-entity-tk* s2 ident cascade)) s edge)
        (remove-entity-tk* s edge cascade)))
    state-map
    (set/intersection (set cascade) (set (keys starting-entity)))))

(>defn remove-entity-tk*
  [state-map ident cascade]
  (let [tables                  (keep (fn [k]
                                        (let [candidate (get state-map k)]
                                          (when (and (map? candidate) (every? map? (vals candidate)))
                                            k))) (keys state-map))
        remove-idents-at-path   (fn [state-map path]
                                  (let [v (get-in state-map path)]
                                    (if (or (eql/ident? v) (every? eql/ident? v))
                                      (merge/remove-ident* state-map ident path)
                                      state-map)))
        candidate-paths         (fn [state-map top-key]     ; allow top-key to be nil to "mean" root node
                                  (map (fn [k]
                                         (if top-key
                                           [top-key k]
                                           [k]))
                                    (keys (get state-map top-key))))
        remove-ident-from-table (fn [state-map table]
                                  (reduce
                                    remove-idents-at-path
                                    state-map
                                    (candidate-paths state-map table)))
        state-without-entity    (->
                                  ;; remove the pointers to the entity
                                  (reduce remove-ident-from-table state-map tables)
                                  ;; remove the top-level edges that point to the entity
                                  (remove-ident-from-table nil)
                                  ;; remove the entity
                                  (dissoc-in ident))
        target-entity           (get-in state-map ident)
        final-state             (cascade-delete* state-without-entity target-entity cascade)]
    final-state))


(comment

  (def state {:fastest-car  [:car/id 1]
              :grandparents [[:person/id 1] [:person/id 2]]
              :denorm       {:level-1 {:level-2 {:a [[:person/id 1] [:person/id 2]] :b [:person/id 1]}}}
              :person/id    {1 {:person/name     "person-1"
                                :person/spouse   [:person/id 2]
                                :person/email    [:email/id 1]
                                :person/cars     [[:car/id 1]]
                                :person/children [[:person/id 3]
                                                  [:person/id 4]
                                                  [:person/id 5]]}
                             2 {:person/name     "person-2"
                                :person/spouse   [:person/id 1]
                                :person/cars     [[:car/id 1]
                                                  [:car/id 2]]
                                :person/children [[:person/id 3]
                                                  [:person/id 4]
                                                  [:person/id 5]]}
                             3 {:person/name "person-3"}
                             4 {:person/name     "person-4"
                                :person/spouse   [:person/id 6]
                                :person/children [:person/id 7]}
                             5 {:person/name "person-5"}
                             6 {:person/id       6
                                :person/name     "person-6"
                                :person/spouse   [:person/id 4]
                                :person/children [:person/id 7]}
                             7 {:person/name "person-7"}}
              :car/id       {1 {:car/model  "model-1"
                                :car/engine [:engine/id 1]}
                             2 {:car/model  "model-2"
                                :car/engine [:engine/id 2]}}
              :engine/id    {1 {:engine/name "engine-1"}
                             2 {:engine/name "engine-2"}}
              :email/id     {1 {:email/provider "Google"}
                             2 {:email/provider "Microsoft"}}})


  ;; TODO
  ;; to-one; this should remove the entity associated email as well
  (remove-entity* state [:person/id 1] #{:person/email})

  ;; TODO
  ;; to-many; this removes the entity associated cars as well
  (remove-entity* state [:person/id 2] #{:person/cars})

  ;; TODO
  ;; to-one; this should remove the entity associated email as well
  (remove-entity* state [:person/id 1] #{:person/email :person/cars})

  ;; TODO
  ;; this should remove the associated children and spouse recursively
  ;; which means it should also delete children of children and their spouse
  (remove-entity* state [:person/id 1] #{:person/children :person/spouse})


  '())




;;============================================================================


(>defn remove-edge*
  ([state-map path-to-edge]
   [map? vector? => any?]
   (remove-edge* state-map path-to-edge #{}))

  ;; TODO implement cascading
  ([state-map path-to-edge cascade]
   [map? vector? (s/coll-of keyword? :kind set?) => map?]
   (if (eql/ident? (clojure.core/get-in state-map path-to-edge))
     (assoc-in state-map path-to-edge {})
     state-map)))


(comment


  '())

;;============================================================================

;; TODO clarify the exact usage
(>defn sort-idents-by
  "
  Intended to be used as
   ```
   (sort-idents-by :entity/field vector-of-idents)

   ```

  Can facilitate:
  ```
  (swap! state update-in [:entity 1 :list] sort-idents-by :list/field)
  ```
  "
  [entity-field vector-of-idents]
  [keyword? vector? => any?]
  (sort-by second vector-of-idents))

(comment

  (def state (atom {:grandparents [[:person/id 3] [:person/id 2]]
                    :person/id    {1 {:person/name     "person-1"
                                      :person/children [[:person/id 3]
                                                        [:person/id 9]
                                                        [:person/id 5]]}
                                   2 {:person/name "person-2"
                                      :person/cars [[:car/id 1]
                                                    [:car/id 2]]}}
                    :car/id       {1 {:car/model "model-1"}
                                   2 {:car/model "model-2"}}}))


  (sort-by second (clojure.core/get-in @state [:person/id 1 :person/children]))

  (swap! state update-in [:person/id 1 :person/children] sort-idents-by :person/id)

  '())

;============================================================================

(defn ui->props
  "Obtain a tree of props for a UI instance from the current application state. Useful in mutations where you want
  to denormalize an entity from the state database. `this` can often be obtained from the mutation `env` at the
  `:component` key."
  ([this]
   (ui->props (comp/component->state-map this) (comp/react-type this) (comp/get-ident this)))
  ([state-map component-class ident]
   (fdn/db->tree (comp/get-query component-class state-map) (get-in state-map ident) state-map)))


(comment

  '())


;============================================================================

;;MAYBE These might belong in mutation ns???
(defn update-caller!
  "Runs clojure.core/update on the table entry in the state database that corresponds
   to the mutation caller (which can be explicitly set via `:ref` when calling `transact!`).

   Equivalent to `(swap! (:state env) update-in (:ref env) ...)`."
  [{:keys [state ref] :as mutation-env} & args]
  (apply swap! state update-in ref args))


(comment


  (let [mutation-env {:ref   [:person/id 1]
                      :state (atom {:person/id {1
                                                {:person/id 1 :person/name "Dad"}}})}]
    (apply swap! (:state mutation-env) update-in (:ref mutation-env)
      (vector assoc :person/name "Mom")))



  (let [mutation-env {:ref   [:person/id 1]
                      :state (atom {:person/id {1
                                                {:person/id 1 :person/name "Dad"}}})}]
    (update-caller! mutation-env
      assoc :person/name "Mom"))




  '())

;============================================================================

;;MAYBE These might belong in mutation ns???
(defn update-caller-in!
  "Like swap! but starts at the ref from `env`, adds in supplied `path` elements
  (resolving across idents if necessary). Finally runs an update-in on that resultant
  path with the given `args`.

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



(comment

  (let [state (atom {:person/id {1 {:person/id       1 :person/name "Dad"
                                    :person/children [[:person/id 2] [:person/id 3]]}
                                 2 {:person/id 2 :person/name "Son"}
                                 3 {:person/id 3 :person/name "Daughter"}}})
        ref   [:person/id 1]
        path  (tree-path->db-path @state (into ref [:person/id 2]))
        args  (vector assoc :person/name "Mom")]

    (if (and path (get-in @state path))
      (apply swap! state update-in path args)
      @state))





  (let [state (atom {:person/id {1 {:person/id       1 :person/name "Dad"
                                    :person/children [[:person/id 2] [:person/id 3]]}
                                 2 {:person/id 2 :person/name "Son"}
                                 3 {:person/id 3 :person/name "Daughter"}}})
        ref   [:person/id 1]
        path  (tree-path->db-path @state (into ref [:person/id 2]))
        args  (vector assoc :person/name "Mom")]

    (and path (get-in @state path)))




  ;;;;;


  '())

;============================================================================

;; TODO: Untested...make up an env with a state atom and see if it works in clj/cljs

#?(:clj
   (defmacro swap!->
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

